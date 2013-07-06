package com.sbbs.me.android.loader;

import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import android.content.Context;
import android.util.Log;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;

public class SbbsCodeTreeLoader extends 
		BaseLoader<TreeEntry> {

	byte repoType;
	String sha;

	public SbbsCodeTreeLoader(Context context, 
			byte repoType, String sha) {
		super(context);
		this.repoType = repoType;
		this.sha = sha;
	}
	
	@Override
	public List<TreeEntry> loadInBackground(){
		String userName = "zhuangbiaowei";
		String repoName = "sbbsme";
		if (repoType == 0) {
			
		} else if (repoType == 1) {
			userName = "rarnu";
			repoName = "root-tools";
		}
		List<TreeEntry> list = null;
		try {
			list = SbbsMeAPI.getCodeTree(userName,
					repoName, sha);
		} catch(Exception e) {
			Log.e("getCodeTree", e.getMessage());
		}
		return list;
	}
}
