package com.sbbs.me.android.database;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import com.sbbs.me.android.consts.FieldDefine;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;

public class GithubUtils {

	public static void closeDatabase(Context context) {
		context.getContentResolver().query(
				ContentUris.withAppendedId(GithubProvider.CONTENT_URI,
						GithubProvider.ACTION_CLOSEDATABASE), null,
				null, null,null);
	}
	
	public static void newDatabase(Context context) {
		context.getContentResolver().query(
				ContentUris.withAppendedId(GithubProvider.CONTENT_URI,
						GithubProvider.ACTION_NEWDATABASE), null,
				null, null, null);
	}
	
	public static List<TreeEntry> getTreeList(Context context, String userName, 
			String repoName, String sha) {
		List<TreeEntry> treeList = null;
		if (!userName.equals("") && !repoName.equals("")
				&& !sha.equals("")) {
			String where = "UserName=? and RepoName=? and Sha=?";
			String sort = "Order desc";
			Cursor c = context.getContentResolver().query(
					ContentUris.withAppendedId(GithubProvider.CONTENT_URI,
							GithubProvider.ACTION_TREELIST),new String[] {userName,
						repoName, sha}, where, null, sort);
			if (c != null) {
				treeList = new ArrayList<TreeEntry>();
				c.moveToFirst();
				while (!c.isAfterLast()) {
					TreeEntry entry = new TreeEntry();
					entry.setPath(c.getColumnName(c.
							getColumnIndex(FieldDefine.TreeEntryFields[4])));
					entry.setType(c.getColumnName(c.
							getColumnIndex(FieldDefine.TreeEntryFields[5])));
					treeList.add(entry);
					c.moveToNext();
				}
			}
		}
		return treeList;
	}
	
	public static void saveTreeList() {
		//TO DO...
	}
}
