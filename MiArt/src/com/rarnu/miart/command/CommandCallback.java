package com.rarnu.miart.command;

public interface CommandCallback {
    void onReadLine(String line);

    void onReadError(String line);

    void onCommandFinish();
}
