package net.sf.saxon.s9api.streams;

import net.sf.saxon.s9api.XdmItem;
import java.util.function.IntFunction;
import java.util.stream.Stream;
public abstract class ProxyStream<T extends XdmItem> implements Stream<T> {
    // Stream<T> base;
/*
    public FixxStream(Stream<T> base) {
        this.base = base;
    }
    */
    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        return null;
    }
}



