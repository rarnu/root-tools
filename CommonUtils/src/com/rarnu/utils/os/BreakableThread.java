package com.rarnu.utils.os;

public class BreakableThread extends Thread {

    public interface RunningCallback {
        boolean getRunningState();
    }

    private RunningCallback callback = null;

    public void setRunningCallback(RunningCallback callback) {
        this.callback = callback;
    }

    public RunningCallback getRunningCallback() {
        return callback;
    }

    public BreakableThread(RunningCallback callback) {
        super();
        setRunningCallback(callback);
    }
}
