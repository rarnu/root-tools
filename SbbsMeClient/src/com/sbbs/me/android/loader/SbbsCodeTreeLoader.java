package com.sbbs.me.android.loader;

import static org.eclipse.egit.github.core.TreeEntry.TYPE_TREE;

import java.util.HashMap;
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
	HashMap<String, String> parentSha;

	public SbbsCodeTreeLoader(Context context, 
			byte repoType, String sha) {
		super(context);
		this.repoType = repoType;
		this.sha = sha;
		this.parentSha = new HashMap<String, String>();
	}
	
	public void setSha(String sha) {
		this.sha = sha;
	}
	
	public void setParentSha(HashMap<String, String> sha) {
		this.parentSha = sha;
	}
	
	@Override
	public List<TreeEntry> loadInBackground(){
		String userName = "zhuangbiaowei";
		String repoName = "sbbsme";
		if (repoType == 1) {
			userName = "rarnu";
			repoName = "root-tools";
		}
		List<TreeEntry> list = null;
		if (sha != null) {
			Log.e("load tree with sha=>", sha);
		} else {
			Log.e("current null", "egg ache");
		}
		try {
			list = SbbsMeAPI.getCodeTree(userName,
					repoName, sha, getContext());
		} catch(Exception e) {
			Log.e("getCodeTree", e.getMessage());
		}
		if (sha != null && parentSha.get(sha) != null) {
			Log.e("current sha parent", parentSha.get(sha));
		} else {
			Log.e("current sha parent", "null");
		}
		if (list != null && sha != null 
				&& parentSha.containsKey(sha)) {
			TreeEntry parent = new TreeEntry();
			parent.setSha(parentSha.get(sha));
			parent.setPath("..");
			parent.setType(TYPE_TREE);
			list.add(0, parent);
		}
		return list;
	}
}
