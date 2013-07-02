package com.rarnu.utils.ex;

public class DownloadInfoEx {

	public int threadId;
	public int startPos;
	public int endPos;
	public int compeleteSize;
	public String url;

	public DownloadInfoEx() {
		
	}
	
	public DownloadInfoEx(int threadId, int startPos, int endPos,
			int compeleteSize, String url) {
		this.threadId = threadId;
		this.startPos = startPos;
		this.endPos = endPos;
		this.compeleteSize = compeleteSize;
		this.url = url;
	}

	@Override
	public String toString() {
		return "DownloadInfo [threadId=" + threadId
				+ ", startPos=" + startPos + ", endPos=" + endPos
				+ ", compeleteSize=" + compeleteSize + "]";
	}

}
