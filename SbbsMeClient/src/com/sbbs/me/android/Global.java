package com.sbbs.me.android;

import java.util.List;

import org.eclipse.egit.github.core.Repository;

import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeMessage;

public class Global {

	public static List<SbbsMeBlock> listArticle = null;
	public static List<SbbsMeMessage> listMessage = null;
	public static List<Repository> listRepos = null;

	public static void releaseAll() {
		if (listArticle != null) {
			listArticle.clear();
		}
		if (listMessage != null) {
			listMessage.clear();
		}
		if (listRepos != null) {
			listRepos.clear();
		}
	}
}
