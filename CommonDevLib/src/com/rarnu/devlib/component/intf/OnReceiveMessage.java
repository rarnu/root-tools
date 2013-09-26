package com.rarnu.devlib.component.intf;

public interface OnReceiveMessage {
    void onStateChange(boolean operating);

    void onProgress(String name, int position, int total);

    void onMutaxMessage(boolean operating);
}