package com.rarnu.devlib.network;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class SocketServer {

    private ServerSocket server;
    private boolean running;
    private SocketServerCallback callback;
    private String endChar;

    public SocketServer(final SocketServerCallback callback, final String endChar) {
        this.callback = callback;
        this.endChar = endChar;
    }

    public void startListen(final int port) {
        new Thread(new Runnable() {

            @Override
            public void run() {
                try {
                    running = true;
                    server = new ServerSocket(port);
                    while (true) {
                        if (!running) {
                            break;
                        }
                        Socket client = server.accept();
                        new Thread(new InnerSocket(client, callback, endChar)).start();
                    }
                } catch (IOException e) {
                    if (callback != null) {
                        callback.onError(e.getMessage());
                    }
                }

            }
        }).start();

    }

    public void stopListen() {
        running = false;
        try {
            server.close();
        } catch (IOException e) {
        }
    }

}
