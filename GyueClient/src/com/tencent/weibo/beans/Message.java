package com.tencent.weibo.beans;

/**
 * 微博内容
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> bywyu
 */
public class Message {
	
	private int ret = 0;
	private String msg = "";
	private int Errcode = 0;
	private Data data = new Data();
	public int getRet() {
		return ret;
	}
	public void setRet(int ret) {
		this.ret = ret;
	}
	public String getMsg() {
		return msg;
	}
	public void setMsg(String msg) {
		this.msg = msg;
	}
	public int getErrcode() {
		return Errcode;
	}
	public void setErrcode(int errcode) {
		Errcode = errcode;
	}
	public Data getData() {
		return data;
	}
	public void setData(Data data) {
		this.data = data;
	}
}
