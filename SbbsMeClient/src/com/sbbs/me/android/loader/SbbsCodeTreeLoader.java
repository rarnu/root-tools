package com.sbbs.me.android.loader;

import static org.eclipse.egit.github.core.TreeEntry.TYPE_TREE;

import java.util.HashMap;
import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.database.GithubUtils;

public class SbbsCodeTreeLoader extends BaseLoader<TreeEntry> {

	int repoType;
	String sha;
	HashMap<String, String> parentSha;
	boolean refresh;

	public SbbsCodeTreeLoader(Context context, int repoType, String sha) {
		super(context);
		this.repoType = repoType;
		this.sha = sha;
		this.parentSha = new HashMap<String, String>();
		this.refresh = false;
	}

	public void setSha(String sha) {
		this.sha = sha;
	}

	public void setParentSha(HashMap<String, String> sha) {
		this.parentSha = sha;
	}
	
	public void setRefresh(boolean refresh) {
		this.refresh = refresh;
	}

	@Override
	public List<TreeEntry> loadInBackground() {
		String userName = "zhuangbiaowei";
		String repoName = "sbbsme";
		if (repoType == 1) {
			userName = "rarnu";
			repoName = "root-tools";
		}
		List<TreeEntry> list = null;
		try {
			list = GithubUtils.getTreeList(getContext(), repoName, sha);
			if (list == null || list.size() == 0 || refresh) {
				list = SbbsMeAPI.getCodeTree(userName, repoName, sha,
						getContext());
				GithubUtils.saveTreeList(getContext(), list, sha, repoName);
			}
		} catch (Exception e) {

		}
		if (list != null && sha != null && parentSha.containsKey(sha)) {
			TreeEntry parent = new TreeEntry();
			parent.setSha(parentSha.get(sha));
			parent.setPath("..");
			parent.setType(TYPE_TREE);
			list.add(0, parent);
		}
		return list;
	}
}
