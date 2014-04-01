package com.rarnu.utils.socket;

public interface SocketClientCallback {
    void onCallback(String msg);

    void onError(String msg);

    /**
     * @param fileName
     * @param total
     * @param progress
     * @param status   0: start<br/>1: end<br/>2: progress
     */
    void onSendFile(int id, String fileName, long total, long progress, int status);
}
