package com.sbbs.me.android.api;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.eclipse.egit.github.core.Blob;
import org.eclipse.egit.github.core.Repository;
import org.eclipse.egit.github.core.RepositoryCommit;
import org.eclipse.egit.github.core.TreeEntry;
import org.eclipse.egit.github.core.client.GitHubClient;
import org.eclipse.egit.github.core.service.CommitService;
import org.eclipse.egit.github.core.service.DataService;
import org.eclipse.egit.github.core.service.RepositoryService;
import org.json.JSONArray;
import org.json.JSONObject;

import android.content.Context;
import android.util.Log;

import com.rarnu.utils.HttpRequest;
import com.rarnu.utils.common.HttpRequestResponseData;
import com.sbbs.me.android.utils.Config;

public class SbbsMeAPI {

	private static final String BASE_URL = "http://sbbs.me/api/";
	private static HttpRequestResponseData cookieData = null;

	public static boolean isLogin() {
		return cookieData != null;
	}

	/**
	 * login
	 * 
	 * @param uid
	 * @param name
	 * @param accountType
	 * @param avatar
	 * @return
	 * @throws Exception
	 */
	public static boolean login(String uid, String name, String accountType,
			String avatar) throws Exception {
		name = URLEncoder.encode(name, HTTP.UTF_8);
		avatar = URLEncoder.encode(avatar, HTTP.UTF_8);

		Log.e("SbbsMeAPI", String.format("uid:%s, name:%s, at:%s, avater:%s",
				uid, name, accountType, avatar));

		List<BasicNameValuePair> param = new ArrayList<BasicNameValuePair>();
		param.add(new BasicNameValuePair("account_type", accountType));
		param.add(new BasicNameValuePair("avatar", avatar));
		cookieData = HttpRequest.postWithHeader(
				BASE_URL + String.format("login/%s/%s", uid, name), param,
				null, HTTP.UTF_8);
		boolean loginStatus = false;
		try {
			JSONObject json = new JSONObject(cookieData.data);
			String id = json.getString("Id");
			loginStatus = id.equals(uid);
		} catch (Exception e) {

		}
		return loginStatus;
	}

	public static void logout() {
		cookieData = null;
	}

	/**
	 * do NOT need login
	 * 
	 * @return
	 * @throws Exception
	 */
	public static List<SbbsMeBlock> getArticles() throws Exception {
		List<SbbsMeBlock> list = null;
		String ret = HttpRequest.get(BASE_URL + "articles", "", HTTP.UTF_8);
		JSONArray jArr = new JSONArray(ret);
		if (jArr != null && jArr.length() != 0) {
			list = new ArrayList<SbbsMeBlock>();
			for (int i = 0; i < jArr.length(); i++) {
				list.add(SbbsMeBlock.fromJson(jArr.getJSONObject(i)));
			}
		}
		return list;
	}

	/**
	 * do NOT need login
	 * 
	 * @param id
	 * @return
	 * @throws Exception
	 */
	public static SbbsMeArticle getArticle(String id) throws Exception {
		SbbsMeArticle article = null;
		String ret = HttpRequest
				.get(BASE_URL + "article/" + id, "", HTTP.UTF_8);
		Log.e("getArticle", ret);
		article = SbbsMeArticle.fromJson(new JSONObject(ret));
		return article;
	}

	@Deprecated
	public static List<Repository> getRepos() {
		List<Repository> list = null;
		RepositoryService service = new RepositoryService();
		Repository mobileRepo = new Repository();
		Repository webRepo = new Repository();
		try {
			mobileRepo = service.getRepository("rarnu", "root-tools");
			webRepo = service.getRepository("zhuangbiaowei", "sbbsme");
		} catch (Exception e) {
		}
		Log.e("getMobileRepo", mobileRepo.getName());
		Log.e("getWebRepo", webRepo.getName());
		list = new ArrayList<Repository>();
		list.add(mobileRepo);
		list.add(webRepo);
		return list;
	}

	public static List<TreeEntry> getCodeTree(String userName, String repoName,
			String sha, Context context) throws Exception {
		List<TreeEntry> list = null;
		GitHubClient client = new GitHubClient();
		client.setOAuth2Token(Config.getGithubAccessToken(context));
		RepositoryService repoService = new RepositoryService(client);
		Repository repo = repoService.getRepository(userName, repoName);
		CommitService commitService = new CommitService();
		if (sha == null) {
			List<RepositoryCommit> repoCommits = commitService.getCommits(repo);
			sha = repoCommits.get(0).getSha();
		}
		DataService dataService = new DataService();
		list = dataService.getTree(repo, sha).getTree();

		return list;
	}

	public static Blob getCodeView(String userName, String repoName,
			String sha, Context context) throws Exception {
		GitHubClient client = new GitHubClient();
		client.setOAuth2Token(Config.getGithubAccessToken(context));
		RepositoryService repoService = new RepositoryService(client);
		Repository repo = repoService.getRepository(userName, repoName);
		DataService dataService = new DataService();
		Blob blob = dataService.getBlob(repo, sha);
		return blob;
	}

	/**
	 * do need login
	 */
	public static List<SbbsMeMessage> getRecentMsg() throws Exception {
		HttpRequestResponseData ret = HttpRequest.getWithData(
				BASE_URL + "msgs", "", cookieData.cookie, HTTP.UTF_8);
		JSONArray jarrData = new JSONArray(ret.data);
		List<SbbsMeMessage> list = null;
		if (jarrData != null && jarrData.length() != 0) {
			list = new ArrayList<SbbsMeMessage>();
			for (int i = 0; i < jarrData.length(); i++) {
				list.add(SbbsMeMessage.fromString(jarrData.getString(i)));
			}
		}
		return list;
	}

	/**
	 * do need login
	 * 
	 * @return
	 * @throws Exception
	 */
	public static int getRecentMsgCount() throws Exception {
		HttpRequestResponseData ret = HttpRequest.getWithData(BASE_URL
				+ "msgs/count", "", cookieData.cookie, HTTP.UTF_8);
		int retInt = 0;
		try {
			retInt = Integer.parseInt(ret.data);
		} catch (Exception e) {

		}
		return retInt;
	}

	/**
	 * do need login
	 * 
	 * @param subject
	 * @param format
	 * @param txtBody
	 * @param isPublic
	 * @param tags
	 * @return blockId / "please login"
	 */
	public static String addNewArticle(String subject, String format,
			String txtBody, boolean isPublic, String[] tags) {
		// curl http://sbbs.me/api/article -d
		// "subject=123&format=Markdown&txtBody=test&public=1&tags=1,2,3"
		String tagStr = "";
		if (tags != null && tags.length != 0) {
			for (String t : tags) {
				tagStr += t + ",";
			}
		}
		tagStr = tagStr.substring(0, tagStr.length() - 1);
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("subject", subject));
		params.add(new BasicNameValuePair("format", format));
		params.add(new BasicNameValuePair("txtBody", txtBody));
		params.add(new BasicNameValuePair("public", isPublic ? "1" : "0"));
		params.add(new BasicNameValuePair("tags", tagStr));
		HttpRequestResponseData ret = HttpRequest.postWithHeader(BASE_URL
				+ "article", params, cookieData.cookie, HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("addNewArticle", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @param blockId
	 * @param text
	 * @return "OK" / "please login" / "another user"
	 */
	public static String appendBlock(String blockId, String text) {
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("text", text));
		HttpRequestResponseData ret = HttpRequest.postWithHeader(BASE_URL
				+ "append_block/b" + blockId, params, cookieData.cookie,
				HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("appendBlock", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @param blockId
	 * @param text
	 * @param title
	 * @return "please login" / "OK"
	 */
	public static String commentBlock(String blockId, String text, String title) {
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("text", text));
		params.add(new BasicNameValuePair("comment_type", "#00f"));
		params.add(new BasicNameValuePair("comment_title", "comment:" + title));
		HttpRequestResponseData ret = HttpRequest.postWithHeader(BASE_URL
				+ "comment_block/b" + blockId, params, cookieData.cookie,
				HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("commentBlock", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @param blockId
	 * @param text
	 * @return "please login" / "OK"
	 */
	public static String editBlock(String blockId, String text) {
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("text", text));
		HttpRequestResponseData ret = HttpRequest.postWithHeader(BASE_URL
				+ "edit_block/b" + blockId, params, cookieData.cookie,
				HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("editBlock", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @param blockId
	 * @return "please login" / "OK" / "OK_ALL"
	 */
	public static String deleteBlock(String blockId) {
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("id", "b" + blockId));
		HttpRequestResponseData ret = HttpRequest.postWithHeader(BASE_URL
				+ "delete_block", params, cookieData.cookie, HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("deleteBlock", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @param myUserId
	 * @param followUserId
	 * @return "OK"
	 */
	public static String followUser(String myUserId, String followUserId) {
		// curl http://sbbs.me/follow_user/1756787011 -d from_user_id=1391292644
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("from_user_id", myUserId));
		HttpRequestResponseData ret = HttpRequest.postWithHeader(BASE_URL
				+ "follow_user/" + followUserId, params, cookieData.cookie,
				HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("followUser", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @param myUserId
	 * @param unfollowUserId
	 * @return "OK"
	 */
	public static String unfollowUser(String myUserId, String unfollowUserId) {
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("from_user_id", myUserId));
		HttpRequestResponseData ret = HttpRequest.postWithHeader(BASE_URL
				+ "unfollow_user/" + unfollowUserId, params, cookieData.cookie,
				HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("followUser", retStr);
		}
		return retStr;
	}

	/**
	 * do NOT need login
	 * 
	 * @param blockId
	 * @return
	 */
	public static SbbsMeBlock getBlock(String blockId) {

		SbbsMeBlock block = null;
		String ret = HttpRequest.get(BASE_URL + "block/b" + blockId, "",
				HTTP.UTF_8);
		Log.e("getBlock", ret);
		try {
			block = SbbsMeBlock.fromJson(new JSONObject(ret));
		} catch (Exception e) {
		}
		return block;
	}

	/**
	 * do NOT need login
	 * 
	 * @param myUserId
	 * @param otherUserId
	 * @return 0: no relationship<br>
	 *         1: I followed other<br>
	 *         2: other followed me<br>
	 *         3: both
	 */
	public static int getFollowStatus(String myUserId, String otherUserId) {
		String ret = HttpRequest.get(BASE_URL + "follow/" + myUserId + "/"
				+ otherUserId, "", HTTP.UTF_8);
		int retInt = 0;
		if ((ret != null) && (!ret.equals(""))) {
			if (ret.equals("both")) {
				retInt = 3;
			} else if (ret.equals("to")) {
				retInt = 2;
			} else if (ret.equals("from")) {
				retInt = 1;
			}
		}
		return retInt;
	}

	/**
	 * do NOT need login
	 * 
	 * @param userId
	 * @return
	 */
	public static SbbsMeUser getUser(String userId) {
		SbbsMeUser user = null;
		String ret = HttpRequest.get(BASE_URL + "user/" + userId, "",
				HTTP.UTF_8);
		Log.e("getUser", ret);
		try {
			user = SbbsMeUser.fromJson(new JSONObject(ret));
		} catch (Exception e) {
		}
		return user;
	}
}
