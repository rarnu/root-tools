package com.rarnu.utils.socket;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class MessageSocketServer {

    private ServerSocket server;
    private boolean running;
    private SocketServerCallback callback;
    private String endChar;
    private int port;

    public MessageSocketServer(final SocketServerCallback callback, final int port, final String endChar) {
        this.callback = callback;
        this.endChar = endChar;
        this.port = port;
    }

    public void startListen() {
        new Thread(new Runnable() {

            @Override
            public void run() {
                try {
                    running = true;
                    server = new ServerSocket(port);
                    while (running) {
                        Socket client = server.accept();
                        new Thread(new MessageInnerSocket(client, callback, endChar)).start();
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
