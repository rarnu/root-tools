package com.rarnu.tools.root.common;

public class MemoryInfo {

    public int Total;
    public int Used;
    public int Free;
    public int Shared;
    public int Buffer;

    public String toString() {
        return String.format("total: %dM, used: %dM, free: %dM, shared: %dM, buffer: %dM", Total, Used, Free, Shared, Buffer);
    }
}
