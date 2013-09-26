package com.rarnu.devlib.network;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

class InnerSocket implements Runnable {

    private Socket client;
    private String listMsg;
    private SocketServerCallback callback;
    private String endChar;

    public InnerSocket(Socket client, SocketServerCallback callback, String endChar) {
        this.client = client;
        this.callback = callback;
        this.endChar = endChar;
    }

    public void run() {
        DataInputStream input;
        DataOutputStream output;
        try {
            input = new DataInputStream(client.getInputStream());
            output = new DataOutputStream(client.getOutputStream());
            while (true) {
                listMsg = input.readUTF();
                if (listMsg != null) {
                    if (callback != null) {
                        callback.onReceive(listMsg);
                    }
                    output.writeUTF(listMsg);
                    if (listMsg.equals(endChar)) {
                        break;
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}