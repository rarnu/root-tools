package com.sbbs.me.android;

import java.util.List;

import org.eclipse.egit.github.core.Repository;

import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeTag;

public class Global {

	public static boolean canExit = false;
	public static boolean autoRefreshTag = false;
	public static boolean autoLoadArticleTag = false;
	public static boolean autoCommentRefreshTag = false;
	public static SbbsMeArticle passArticle = null;
	public static List<SbbsMeBlock> listArticle = null;
	public static List<SbbsMeTag> listTags = null;
	public static List<Repository> listRepos = null;	

	public static void releaseAll() {
		if (listArticle != null) {
			listArticle.clear();
		}
		if (listTags != null) {
			listTags.clear();
		}
		if (listRepos != null) {
			listRepos.clear();
		}
	}
}
