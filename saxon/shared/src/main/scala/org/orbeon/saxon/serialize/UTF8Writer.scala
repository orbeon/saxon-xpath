package org.orbeon.saxon.serialize

import java.io.{IOException, OutputStream, Writer}


object UTF8Writer {
  private val MIN_BUF_LEN = 32
  private val DEFAULT_BUF_LEN = 4000
  private[serialize] val SURR1_FIRST = 0xD800
  private[serialize] val SURR1_LAST = 0xDBFF
  private[serialize] val SURR2_FIRST = 0xDC00
  private[serialize] val SURR2_LAST = 0xDFFF
}

final class UTF8Writer(var _out: OutputStream, val bufferLength: Int) extends Writer {

  var buffLength = bufferLength
  private[serialize] var _surrogate = 0
  var _outBuffer: Array[Byte] = null
  var _outBufferLast: Int = 0

  var _outPtr = 0
  if (buffLength < UTF8Writer.MIN_BUF_LEN)
    buffLength = UTF8Writer.MIN_BUF_LEN
  _outBuffer = new Array[Byte](buffLength)
  _outBufferLast = buffLength - 4
  _outPtr = 0

  def this(out: OutputStream) =
    this(out, UTF8Writer.DEFAULT_BUF_LEN)

  @throws[IOException]
  def close(): Unit =
    if (_out != null) {
      _flushBuffer()
      _outBuffer = null
      _out.close()
      _out = null
      if (_surrogate != 0) {
        val code = _surrogate
        _surrogate = 0
        throwIllegal(code)
      }
    }

  @throws[IOException]
  def flush(): Unit = {
    _flushBuffer()
    _out.flush()
  }

  @throws[IOException]
  override def write(cbuf: Array[Char]): Unit =
    write(cbuf, 0, cbuf.length)

  @throws[IOException]
  def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    var offInt = off
    var lenInt = len
    if (lenInt < 2) {
      if (lenInt == 1)
        write(cbuf(offInt))
      return
    }

    if (_surrogate > 0) {
      val second = cbuf({
        offInt += 1
        offInt - 1
      })
      lenInt -= 1
      write(_convertSurrogate(second))
    }

    var outPtr = _outPtr
    val outBuf = _outBuffer
    val outBufLast = _outBufferLast

    lenInt += off
    var exitOutputLoop = false
    while (! exitOutputLoop && offInt < len) {
      if (outPtr >= outBufLast) {
        _out.write(outBuf, 0, outPtr)
        outPtr = 0
      }
      var c: Int = cbuf({
        offInt += 1
        offInt - 1
      })

      var continueOutputLoop = false
      if (c < 0x80) {
        outBuf({
          outPtr += 1
          outPtr - 1
        }) = c.toByte

        var maxInCount = len - offInt
        val maxOutCount = outBufLast - outPtr
        if (maxInCount > maxOutCount) maxInCount = maxOutCount
        maxInCount += offInt
        var exitAsciiLoop = false
        while (! exitAsciiLoop) {
          if (offInt >= maxInCount) {
            exitAsciiLoop = true
            continueOutputLoop = true
          } else {
            c = cbuf({
              offInt += 1
              offInt - 1
            })
            if (c >= 0x80)
              exitAsciiLoop = true
            else
              outBuf({
                outPtr += 1
                outPtr - 1
              }) = c.toByte
          }
        } // end ascii_loop
      }

      if (! continueOutputLoop) {
        if (c < 0x800) {
          outBuf({
            outPtr += 1
            outPtr - 1
          }) = (0xc0 | (c >> 6)).toByte
          outBuf({
            outPtr += 1
            outPtr - 1
          }) = (0x80 | (c & 0x3f)).toByte
        } else {

          if (c < UTF8Writer.SURR1_FIRST || c > UTF8Writer.SURR2_LAST) {
            outBuf({
              outPtr += 1
              outPtr - 1
            }) = (0xe0 | (c >> 12)).toByte
            outBuf({
              outPtr += 1
              outPtr - 1
            }) = (0x80 | ((c >> 6) & 0x3f)).toByte
            outBuf({
              outPtr += 1
              outPtr - 1
            }) = (0x80 | (c & 0x3f)).toByte

            // continue output_loop
          } else {
            if (c > UTF8Writer.SURR1_LAST) {
              _outPtr = outPtr
              throwIllegal(c)
            }
            _surrogate = c

            if (offInt >= len) {
              exitOutputLoop = true
            } else {
              c = _convertSurrogate(cbuf({
                offInt += 1
                offInt - 1
              }))
              if (c > 0x10FFFF) {
                _outPtr = outPtr
                throwIllegal(c)
              }
              outBuf({
                outPtr += 1
                outPtr - 1
              }) = (0xf0 | (c >> 18)).toByte
              outBuf({
                outPtr += 1
                outPtr - 1
              }) = (0x80 | ((c >> 12) & 0x3f)).toByte
              outBuf({
                outPtr += 1
                outPtr - 1
              }) = (0x80 | ((c >> 6) & 0x3f)).toByte
              outBuf({
                outPtr += 1
                outPtr - 1
              }) = (0x80 | (c & 0x3f)).toByte
            }
          }
        }
      }
    } // end output_loop
    _outPtr = outPtr
  }

  @throws[IOException]
  override def write(c: Int): Unit = {
    var cInt = c
    if (_surrogate > 0) {
      cInt = _convertSurrogate(c)
    } else if (cInt >= UTF8Writer.SURR1_FIRST && cInt <= UTF8Writer.SURR2_LAST) {
      if (cInt > UTF8Writer.SURR1_LAST)
        throwIllegal(cInt)
      _surrogate = cInt
      return
    }
    if (_outPtr >= _outBufferLast)
      _flushBuffer()
    if (cInt < 0x80) {
      _outBuffer({
        _outPtr += 1
        _outPtr - 1
      }) = cInt.toByte
    } else {
      var ptr = _outPtr
      if (cInt < 0x800) {
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0xc0 | (cInt >> 6)).toByte
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0x80 | (cInt & 0x3f)).toByte
      }
      else if (cInt <= 0xFFFF) {
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0xe0 | (cInt >> 12)).toByte
        _outBuffer({
          ptr += 1;
          ptr - 1
        }) = (0x80 | ((cInt >> 6) & 0x3f)).toByte
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0x80 | (cInt & 0x3f)).toByte
      }
      else {
        if (cInt > 0x10FFFF) throwIllegal(cInt)
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0xf0 | (cInt >> 18)).toByte
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0x80 | ((cInt >> 12) & 0x3f)).toByte
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0x80 | ((cInt >> 6) & 0x3f)).toByte
        _outBuffer({
          ptr += 1
          ptr - 1
        }) = (0x80 | (cInt & 0x3f)).toByte
      }
      _outPtr = ptr
    }
  }

  @throws[IOException]
  override def write(str: String): Unit =
    write(str, 0, str.length)

  @throws[IOException]
  override def write(str: String, off: Int, len: Int): Unit = {
    var _off = off
    var lenInt = len
    if (lenInt < 2) {
      if (lenInt == 1)
        write(str.charAt(_off))
      return
    }
    if (_surrogate > 0) {
      val second = str.charAt({
        _off += 1
        _off - 1
      })
      _off -= 1
      write(_convertSurrogate(second))
    }
    var outPtr = _outPtr
    val outBuf = _outBuffer
    val outBufLast = _outBufferLast
    lenInt += _off
    var exitOutputLoop = false
    while (! exitOutputLoop && _off < lenInt) {
      if (outPtr >= outBufLast) {
        _out.write(outBuf, 0, outPtr)
        outPtr = 0
      }
      var c: Int = str.charAt({
        _off += 1
        _off - 1
      })
      var continueOutputLoop = false
      if (c < 0x80) {
        outBuf({
          outPtr += 1
          outPtr - 1
        }) = c.toByte
        var maxInCount = lenInt - _off
        val maxOutCount = outBufLast - outPtr
        if (maxInCount > maxOutCount)
          maxInCount = maxOutCount
        maxInCount += _off
        var exitAsciiLoop = false
        while (! exitAsciiLoop) {
          if (_off >= maxInCount) {
            // done with max. ascii seq
            exitAsciiLoop = true
            continueOutputLoop = true
          } else {
            c = str.charAt({
              _off += 1
              _off - 1
            })
            if (c >= 0x80)
              exitAsciiLoop = true
            else
              outBuf({
                outPtr += 1
                outPtr - 1
              }) = c.toByte
          }
        }
      }
      if (! continueOutputLoop) {
        if (c < 0x800) {
          outBuf({
            outPtr += 1
            outPtr - 1
          }) = (0xc0 | (c >> 6)).toByte
          outBuf({
            outPtr += 1
            outPtr - 1
          }) = (0x80 | (c & 0x3f)).toByte
        } else {
          if (c < UTF8Writer.SURR1_FIRST || c > UTF8Writer.SURR2_LAST) {
            outBuf({
              outPtr += 1
              outPtr - 1
            }) = (0xe0 | (c >> 12)).toByte
            outBuf({
              outPtr += 1
              outPtr - 1
            }) = (0x80 | ((c >> 6) & 0x3f)).toByte
            outBuf({
              outPtr += 1
              outPtr - 1
            }) = (0x80 | (c & 0x3f)).toByte
            // continue output_loop
          } else {
            if (c > UTF8Writer.SURR1_LAST) {
              _outPtr = outPtr
              throwIllegal(c)
            }
            _surrogate = c
            if (_off >= lenInt) {
              exitOutputLoop = true
            } else {
              c = _convertSurrogate(str.charAt({
                _off += 1
                _off - 1
              }))
              if (c > 0x10FFFF) {
                _outPtr = outPtr
                throwIllegal(c)
              }
              outBuf({
                outPtr += 1;
                outPtr - 1
              }) = (0xf0 | (c >> 18)).toByte
              outBuf({
                outPtr += 1;
                outPtr - 1
              }) = (0x80 | ((c >> 12) & 0x3f)).toByte
              outBuf({
                outPtr += 1;
                outPtr - 1
              }) = (0x80 | ((c >> 6) & 0x3f)).toByte
              outBuf({
                outPtr += 1;
                outPtr - 1
              }) = (0x80 | (c & 0x3f)).toByte
            }
          }
        }
      }
    } //end while
    _outPtr = outPtr
  }

  @throws[IOException]
  private def _flushBuffer(): Unit = if (_outPtr > 0 && _outBuffer != null) {
    _out.write(_outBuffer, 0, _outPtr)
    _outPtr = 0
  }

  @throws[IOException]
  private def _convertSurrogate(secondPart: Int): Int = {
    val firstPart = _surrogate
    _surrogate = 0

    if (secondPart < UTF8Writer.SURR2_FIRST || secondPart > UTF8Writer.SURR2_LAST)
      throw new IOException("Broken surrogate pair: first char 0x" + Integer.toHexString(firstPart) + ", second 0x" + Integer.toHexString(secondPart) + "; illegal combination")
    0x10000 + ((firstPart - UTF8Writer.SURR1_FIRST) << 10) + (secondPart - UTF8Writer.SURR2_FIRST)
  }

  @throws[IOException]
  private def throwIllegal(code: Int): Unit =
    if (code > 0x10FFFF)
      throw new IOException("Illegal character point (0x" + Integer.toHexString(code) + ") to output; max is 0x10FFFF as per RFC 3629")
    else if (code >= UTF8Writer.SURR1_FIRST) {
      if (code <= UTF8Writer.SURR1_LAST)
        throw new IOException("Unmatched first part of surrogate pair (0x" + Integer.toHexString(code) + ")")
      else
        throw new IOException("Unmatched second part of surrogate pair (0x" + Integer.toHexString(code) + ")")
    } else
      throw new IOException("Illegal character point (0x" + Integer.toHexString(code) + ") to output")
}