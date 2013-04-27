package com.yugioh.android.intf;

public interface IUpdateIntf {

	void setUpdateFile(String localDir, String localFile);
	void setInProgress(boolean inProgress);
	boolean isInProgress();
}
