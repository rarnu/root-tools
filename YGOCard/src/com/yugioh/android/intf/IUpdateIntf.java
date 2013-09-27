package com.yugioh.android.intf;

public interface IUpdateIntf {

    void setUpdateFile(String localDir, String localFile);

    boolean isInProgress();

    void setInProgress(boolean inProgress);
}
