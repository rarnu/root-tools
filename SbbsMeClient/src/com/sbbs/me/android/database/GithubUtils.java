package com.sbbs.me.android.database;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import android.content.ContentResolver;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;

public class GithubUtils {

	public static void saveTreeList(Context context, List<TreeEntry> list,
			String parentSha, String repo) {
		if (context != null && list != null) {
			if (parentSha == null) {
				parentSha = "";
			}
			ContentResolver cr = context.getContentResolver();
			for (int i = 0; i < list.size(); i++) {
				ContentValues cv = new ContentValues();
				cv.put("sha", list.get(i).getSha());
				cv.put("parent_sha", parentSha);
				cv.put("path", list.get(i).getPath());
				cv.put("type", list.get(i).getType());
				cv.put("repo", repo);
				cr.insert(ContentUris.withAppendedId(
						GithubProvider.CONTENT_URI,
						GithubProvider.ACTION_GITHUB_CACHE), cv);
			}
		}
	}

	public static List<TreeEntry> getTreeList(Context context, String repo,
			String sha) {
		List<TreeEntry> treeList = null;
		if (!repo.equals("")) {
			if (sha == null) {
				sha = "";
			}
			Cursor c = context.getContentResolver()
					.query(ContentUris.withAppendedId(
							GithubProvider.CONTENT_URI,
							GithubProvider.ACTION_GITHUB_CACHE), null,
							"parent_sha=? and repo=?",
							new String[] { sha, repo }, null);
			if (c != null) {
				treeList = new ArrayList<TreeEntry>();
				c.moveToFirst();
				while (!c.isAfterLast()) {
					TreeEntry entry = new TreeEntry();
					entry.setSha(c.getString(c.getColumnIndex("sha")));
					entry.setPath(c.getString(c.getColumnIndex("path")));
					entry.setType(c.getString(c.getColumnIndex("type")));
					treeList.add(entry);
					c.moveToNext();
				}
				c.close();
			}
		}
		return treeList;
	}
}
