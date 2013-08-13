package com.sbbs.me.android.api;

import java.io.Serializable;

public class SbbsMeUserLite implements Serializable {

	private static final long serialVersionUID = 6707997513175038028L;

	public String Id;
	public String Name;

	public SbbsMeUserLite(String id, String name) {
		this.Id = id;
		this.Name = name;
	}

}
