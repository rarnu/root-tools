package com.rarnu.tools.neo.root;

public interface CommandCallback {
    void onReadLine(String line);

    void onReadError(String line);

    void onCommandFinish();
}
