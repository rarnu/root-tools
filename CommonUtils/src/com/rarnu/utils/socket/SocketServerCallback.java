package com.rarnu.utils.socket;

public interface SocketServerCallback {

    void onError(String msg);

    void onReceiveMessage(String msg);

    /**
     *
     * @param fileName
     * @param total
     * @param progress
     * @param status 0: start<br/>1: end<br/>2: progress
     */
    void onReceiveFile(String fileName, long total, long progress, int status);
}
