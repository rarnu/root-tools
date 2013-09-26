package com.rarnu.devlib.network;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;

public class SocketClient {

    private Socket socket;
    private OutputStream os;
    private DataOutputStream dos;
    private DataInputStream dis;
    private SocketClientCallback callback;

    public SocketClient(final String ip, final int port, final SocketClientCallback callback) {
        this.callback = callback;
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    socket = new Socket(ip, port);
                    os = socket.getOutputStream();
                    dos = new DataOutputStream(os);
                    dis = new DataInputStream(socket.getInputStream());
                } catch (Exception e) {
                }
            }
        }).start();
    }

    public void close() {
        try {
            dos.close();
        } catch (IOException e1) {
        }
        try {
            dis.close();
        } catch (IOException e1) {
        }
        if (socket != null) {
            try {
                socket.close();
            } catch (IOException e) {
            }
        }
    }

    public void sendMessage(final String msg) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    dos.writeUTF(msg);
                    String res = dis.readUTF();
                    if (callback != null) {
                        callback.onCallback(res);
                    }
                } catch (Exception e) {
                    if (callback != null) {
                        callback.onError(e.getMessage());
                    }
                }
            }
        }).start();
    }
}
