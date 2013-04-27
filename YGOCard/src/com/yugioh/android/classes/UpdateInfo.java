package com.yugioh.android.classes;

import java.io.Serializable;

@SuppressWarnings("serial")
public class UpdateInfo implements Serializable {

	/**
	 * -1: downloaded
	 * 0:no update
	 * 1:has update
	 */
	private int updateApk;
	private int updateData;
	private int newCard;
	private String apkVersion;

	public int getUpdateApk() {
		return updateApk;
	}

	public void setUpdateApk(int updateApk) {
		this.updateApk = updateApk;
	}

	public int getUpdateData() {
		return updateData;
	}

	public void setUpdateData(int updateData) {
		this.updateData = updateData;
	}

	public int getNewCard() {
		return newCard;
	}

	public void setNewCard(int newCard) {
		this.newCard = newCard;
	}

	public String getApkVersion() {
		return apkVersion;
	}

	public void setApkVersion(String apkVersion) {
		this.apkVersion = apkVersion;
	}

}
