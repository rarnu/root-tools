package com.rarnu.utils.socket;

import java.io.*;
import java.net.Socket;

public class FileSocketClient {

    private SocketClientCallback callback;
    private String ip;
    private int port;
    Socket socket = null;

    public FileSocketClient(final SocketClientCallback callback, final String ip, final int port) {
        this.callback = callback;
        this.ip = ip;
        this.port = port;
    }

    public void sendFile(final String filePath) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    File file = new File(filePath);
                    socket = new Socket(ip, port);
                    DataOutputStream dos = new DataOutputStream(socket.getOutputStream());
                    DataInputStream dis = new DataInputStream(new FileInputStream(filePath));
                    int buffferSize = 1024;
                    byte[] bufArray = new byte[buffferSize];
                    long passedlen = 0L;
                    long len = file.length();
                    dos.writeUTF(file.getName());
                    dos.flush();
                    dos.writeLong(len);
                    dos.flush();
                    if (callback != null) {
                        callback.onSendFile(filePath, len, 0L, 0);
                    }
                    while (true) {
                        int read = 0;
                        if (dis != null) {
                            read = dis.read(bufArray);
                        }
                        passedlen += read;
                        if (read == -1) {
                            break;
                        }
                        dos.write(bufArray, 0, read);
                        if (callback != null) {
                            callback.onSendFile(filePath, len, passedlen, 2);
                        }
                    }
                    dos.flush();
                    dos.close();
                    dis.close();
                    socket.close();
                    if (callback != null) {
                        callback.onSendFile(filePath, len, len, 1);
                    }
                } catch (Exception e) {
                    if (callback != null) {
                        callback.onError(e.getMessage());
                    }
                }
            }
        }).start();

    }

    public void stop() {
        try {
            if (socket != null) {
                socket.close();
            }
        } catch (Exception e) {

        }
    }
}
