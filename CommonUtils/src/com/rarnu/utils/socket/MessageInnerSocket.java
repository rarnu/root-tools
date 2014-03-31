package com.rarnu.utils.socket;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

class MessageInnerSocket implements Runnable {

    private Socket client;
    private String listMsg;
    private SocketServerCallback callback;
    private String endChar;

    public MessageInnerSocket(Socket client, SocketServerCallback callback, String endChar) {
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
                        callback.onReceiveMessage(listMsg);
                    }
                    output.writeUTF(listMsg);
                    if (listMsg.equals(endChar)) {
                        break;
                    }
                }
            }
        } catch (IOException e) {
            if (callback != null) {
                callback.onError(e.getMessage());
            }
        }
    }
}