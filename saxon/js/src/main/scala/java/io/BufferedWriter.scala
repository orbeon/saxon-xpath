/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package java.io


/**
 * Wraps an existing `Writer` and <em>buffers</em> the output. Expensive
 * interaction with the underlying reader is minimized, since most (smaller)
 * requests can be satisfied by accessing the buffer alone. The drawback is that
 * some extra space is required to hold the buffer and that copying takes place
 * when filling that buffer, but this is usually outweighed by the performance
 * benefits.
 *
 * A typical application pattern for the class looks like this:
 *
 *     BufferedWriter buf = new BufferedWriter(new FileWriter(&quot;file.java&quot;));
 *
 * @see BufferedReader
 */
class BufferedWriter
  extends Writer {

  private var out: Writer      = null
  private var buf: Array[Char] = null
  private var pos: Int         = 0

  final private val lineSeparator: String = "\n" // ORBEON: Unclear what it should be in the browser.

  /**
   * Constructs a new `BufferedWriter` with `out` as the writer
   * for which to buffer write operations. The buffer size is set to the
   * default value of 8 KB.
   *
   * @param out
   * the writer for which character writing is buffered.
   */
  def this(out: Writer) = {
    this()
    this.out = out
    buf = new Array[Char](8192)
  }

  /**
   * Constructs a new `BufferedWriter` with `out` as the writer
   * for which to buffer write operations. The buffer size is set to `size`.
   *
   * @param out
   * the writer for which character writing is buffered.
   * @param size
   * the size of the buffer in bytes.
   * @throws IllegalArgumentException
   * if `size <= 0`.
   */
  def this(
    out : Writer,
    size: Int
  )=  {
    this()
    if (size <= 0)
      throw new IllegalArgumentException("size must be > 0")
    this.out = out
    this.buf = new Array[Char](size)
  }
  /**
   * Closes this writer. The contents of the buffer are flushed, the target
   * writer is closed, and the buffer is released. Only the first invocation
   * of close has any effect.
   *
   * @throws IOException
   * if an error occurs while closing this writer.
   */
  override def close(): Unit = {
    if (isClosed)
      return
    var thrown: Throwable = null
    try
      flushInternal()
    catch {
      case e: Throwable =>
        thrown = e
    }
    buf = null
    try out.close()
    catch {
      case e: Throwable =>
        if (thrown == null)
          thrown = e
    }
    out = null
    if (thrown != null)
      throw thrown
  }
  /**
   * Flushes this writer. The contents of the buffer are committed to the
   * target writer and it is then flushed.
   *
   * @throws IOException
   * if an error occurs while flushing this writer.
   */
  override def flush(): Unit = {
    if (isClosed)
      throw new IOException("Writer is closed.")
    flushInternal()
    out.flush()
  }

  /**
   * Flushes the internal buffer.
   */
  private def flushInternal(): Unit = {
    if (pos > 0)
      out.write(buf, 0, pos)
    pos = 0
  }

  /**
   * Indicates whether this writer is closed.
   *
   * @return `true` if this writer is closed, `false` otherwise.
   */
  private def isClosed =
    out == null

  /**
   * Writes a newline to this writer. A newline is determined by the System
   * property "line.separator". The target writer may or may not be flushed
   * when a newline is written.
   *
   * @throws IOException
   * if an error occurs attempting to write to this writer.
   */
  def newLine(): Unit =
    write(lineSeparator, 0, lineSeparator.length)

  /**
   * Writes `count` characters starting at `offset` in
   * `cbuf` to this writer. If `count` is greater than this
   * writer's buffer, then the buffer is flushed and the characters are
   * written directly to the target writer.
   *
   * @param cbuf
   * the array containing characters to write.
   * @param offset
   * the start position in `cbuf` for retrieving characters.
   * @param count
   * the maximum number of characters to write.
   * @throws IndexOutOfBoundsException
   * if `offset < 0` or `count < 0`, or if
   * `offset + count` is greater than the size of
   * `cbuf`.
   * @throws IOException
   * if this writer is closed or another I/O error occurs.
   */
  override def write(
    cbuf  : Array[Char],
    offset: Int,
    count : Int
  ): Unit = {
    if (isClosed)
      throw new IOException("Writer is closed.")
    if (offset < 0 || offset > cbuf.length - count || count < 0)
      throw new IndexOutOfBoundsException
    if (pos == 0 && count >= this.buf.length) {
      out.write(cbuf, offset, count)
      return
    }
    var available = this.buf.length - pos
    if (count < available) available = count
    if (available > 0) {
      System.arraycopy(cbuf, offset, this.buf, pos, available)
      pos += available
    }
    if (pos == this.buf.length) {
      out.write(this.buf, 0, this.buf.length)
      pos = 0
      if (count > available) {
        val _offset = offset + available
        available = count - available
        if (available >= this.buf.length) {
          out.write(cbuf, _offset, available)
          return
        }
        System.arraycopy(cbuf, _offset, this.buf, pos, available)
        pos += available
      }
    }
  }

  /**
   * Writes the character `oneChar` to this writer. If the buffer
   * gets full by writing this character, this writer is flushed. Only the
   * lower two bytes of the integer `oneChar` are written.
   *
   * @param oneChar
   * the character to write.
   * @throws IOException
   * if this writer is closed or another I/O error occurs.
   */
  override def write(oneChar: Int): Unit = {
    if (isClosed)
      throw new IOException("Writer is closed.")
    if (pos >= buf.length) {
      out.write(buf, 0, buf.length)
      pos = 0
    }
    buf({pos += 1; pos - 1}) = oneChar.toChar
  }
  /**
   * Writes `count` characters starting at `offset` in `str`
   * to this writer. If `count` is greater than this writer's buffer,
   * then this writer is flushed and the remaining characters are written
   * directly to the target writer. If count is negative no characters are
   * written to the buffer. This differs from the behavior of the superclass.
   *
   * @param str
   * the non-null String containing characters to write.
   * @param offset
   * the start position in `str` for retrieving characters.
   * @param count
   * maximum number of characters to write.
   * @throws IOException
   * if this writer has already been closed or another I/O error
   * occurs.
   * @throws IndexOutOfBoundsException
   * if `offset < 0` or `offset + count` is greater
   * than the length of `str`.
   */
  override def write(
    str   : String,
    offset: Int,
    count : Int
  ): Unit = {
    if (isClosed)
      throw new IOException("Writer is closed.")
    if (count <= 0) return
    if (offset > str.length - count || offset < 0)
      throw new StringIndexOutOfBoundsException
    if (pos == 0 && count >= buf.length) {
      val chars = new Array[Char](count)
      str.getChars(offset, offset + count, chars, 0)
      out.write(chars, 0, count)
      return
    }
    var available = buf.length - pos
    if (count < available) available = count
    if (available > 0) {
      str.getChars(offset, offset + available, buf, pos)
      pos += available
    }
    if (pos == buf.length) {
      out.write(this.buf, 0, this.buf.length)
      pos = 0
      if (count > available) {
        val _offset = offset + available
        available = count - available
        if (available >= buf.length) {
          val chars = new Array[Char](count)
          str.getChars(_offset, _offset + available, chars, 0)
          out.write(chars, 0, available)
          return
        }
        str.getChars(_offset, _offset + available, buf, pos)
        pos += available
      }
    }
  }
}