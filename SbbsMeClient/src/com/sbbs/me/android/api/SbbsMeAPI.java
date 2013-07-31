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
import android.text.format.DateFormat;
import android.util.Log;

import com.rarnu.utils.DeviceUtilsLite;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.HttpRequest;
import com.rarnu.utils.common.HttpRequestResponseData;
import com.sbbs.me.android.utils.AccountUtils;
import com.sbbs.me.android.utils.Config;

public class SbbsMeAPI {

	public static final String ROOT_URL = "http://sbbs.me/";
	private static final String BASE_URL = "http://sbbs.me/api/";
	private static final String LOG_URL = "http://rarnu.7thgen.info/sbbs/";
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
			String avatar) {
		boolean loginStatus = false;
		try {
			name = URLEncoder.encode(name, HTTP.UTF_8);
			avatar = URLEncoder.encode(avatar, HTTP.UTF_8);

			Log.e("SbbsMeAPI", String.format(
					"uid:%s, name:%s, at:%s, avater:%s", uid, name,
					accountType, avatar));

			List<BasicNameValuePair> param = new ArrayList<BasicNameValuePair>();
			param.add(new BasicNameValuePair("account_type", accountType));
			param.add(new BasicNameValuePair("avatar", avatar));
			cookieData = HttpRequest.postWithHeader(
					BASE_URL + String.format("login/%s/%s", uid, name), param,
					null, HTTP.UTF_8);

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
	public static List<SbbsMeBlock> getArticles() {
		String ret = HttpRequest.get(BASE_URL + "articles", "", HTTP.UTF_8);
		return buildArticleList(ret);
	}

	public static List<SbbsMeBlock> getArticles(int page, int pageSize) {
		String ret = HttpRequest.get(
				BASE_URL + String.format("articles/%d/%d", page, pageSize), "",
				HTTP.UTF_8);
		return buildArticleList(ret);
	}

	private static List<SbbsMeBlock> buildArticleList(String jsonRet) {
		List<SbbsMeBlock> list = null;
		try {
			JSONArray jArr = new JSONArray(jsonRet);
			if (jArr != null && jArr.length() != 0) {
				list = new ArrayList<SbbsMeBlock>();
				for (int i = 0; i < jArr.length(); i++) {
					list.add(SbbsMeBlock.fromJson(jArr.getJSONObject(i)));
				}
			}
		} catch (Exception e) {

		}
		return list;
	}

	public static SbbsMeSideBlocks getSideBlocks(SbbsMeArticle article,
			String blockId) {
		SbbsMeSideBlocks sb = new SbbsMeSideBlocks();
		List<SbbsMeBlock> listLeft = article.left_blocks.get(blockId);
		List<SbbsMeBlock> listRight = article.right_blocks.get(blockId);
		if (listLeft != null && listLeft.size() != 0) {
			sb.leftBlockCount = listLeft.size();
		}
		if (listRight != null && listRight.size() != 0) {
			sb.rightBlockCount = listRight.size();
		}
		return sb;
	}

	/**
	 * do NOT need login
	 * 
	 * @param tagId
	 * @return
	 * @throws Exception
	 */
	public static List<SbbsMeBlock> getArticlesViaTag(String tagId) {
		String ret = HttpRequest.get(BASE_URL + "articles/tag/" + tagId, "",
				HTTP.UTF_8);
		return buildArticleList(ret);
	}

	public static List<SbbsMeBlock> getArticlesViaTag(String tagId, int page,
			int pageSize) {
		String ret = HttpRequest.get(
				BASE_URL
						+ String.format("articles/tag/%s/%d/%d", tagId, page,
								pageSize), "", HTTP.UTF_8);
		return buildArticleList(ret);
	}

	/**
	 * do NOT need login
	 * 
	 * @param id
	 * @return
	 * @throws Exception
	 */
	public static SbbsMeArticle getArticle(String id) {
		SbbsMeArticle article = null;
		try {
			String ret = HttpRequest.get(BASE_URL + "article/" + id, "",
					HTTP.UTF_8);
			Log.e("getArticle", ret);
			article = SbbsMeArticle.fromJson(new JSONObject(ret));
		} catch (Exception e) {

		}
		return article;
	}

	public static List<TreeEntry> getCodeTree(String userName, String repoName,
			String sha, Context context) throws Exception {
		List<TreeEntry> list = null;
		GitHubClient client = new GitHubClient();
		client.setOAuth2Token(Config.getGithubAccessToken(context));
		RepositoryService repoService = new RepositoryService(client);
		Repository repo = repoService.getRepository(userName, repoName);
		CommitService commitService = new CommitService(client);
		if (sha == null) {
			List<RepositoryCommit> repoCommits = commitService.getCommits(repo);
			sha = repoCommits.get(0).getSha();
		}
		DataService dataService = new DataService(client);
		list = dataService.getTree(repo, sha).getTree();

		return list;
	}

	public static Blob getCodeView(String userName, String repoName,
			String sha, Context context) throws Exception {
		GitHubClient client = new GitHubClient();
		client.setOAuth2Token(Config.getGithubAccessToken(context));
		RepositoryService repoService = new RepositoryService(client);
		Repository repo = repoService.getRepository(userName, repoName);
		DataService dataService = new DataService(client);
		Blob blob = dataService.getBlob(repo, sha);
		return blob;
	}

	/**
	 * do need login
	 */
	public static List<SbbsMeMessage> getRecentMsg() {
		List<SbbsMeMessage> list = null;
		try {
			HttpRequestResponseData ret = HttpRequest.getWithHeader(BASE_URL
					+ "msgs", "", cookieData.cookie, HTTP.UTF_8);
			JSONArray jarrData = new JSONArray(ret.data);

			if (jarrData != null && jarrData.length() != 0) {
				list = new ArrayList<SbbsMeMessage>();
				for (int i = 0; i < jarrData.length(); i++) {
					list.add(0, SbbsMeMessage.fromString(jarrData.getString(i)));
				}
			}
		} catch (Exception e) {

		}
		return list;
	}

	/**
	 * do need login
	 * 
	 * @return
	 * @throws Exception
	 */
	public static int getRecentMsgCount() {
		int retInt = 0;

		try {
			HttpRequestResponseData ret = HttpRequest.getWithHeader(BASE_URL
					+ "msgs/count", "", cookieData.cookie, HTTP.UTF_8);
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
			String txtBody, boolean isPublic, String tags) {

		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("subject", subject));
		params.add(new BasicNameValuePair("format", format));
		params.add(new BasicNameValuePair("txtBody", txtBody));
		params.add(new BasicNameValuePair("public", isPublic ? "1" : "0"));
		params.add(new BasicNameValuePair("tags", tags));
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
		try {
			String ret = HttpRequest.get(BASE_URL + "block/b" + blockId, "",
					HTTP.UTF_8);
			Log.e("getBlock", ret);
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
		try {
			String ret = HttpRequest.get(BASE_URL + "user/" + userId, "",
					HTTP.UTF_8);
			Log.e("getUser", ret);
			user = SbbsMeUser.fromJson(new JSONObject(ret));
		} catch (Exception e) {

		}
		return user;
	}

	/**
	 * do NOT need login
	 * 
	 * @return
	 */
	public static List<SbbsMeTag> getTags() {
		List<SbbsMeTag> list = null;
		try {
			String ret = HttpRequest.get(BASE_URL + "tags", "", HTTP.UTF_8);
			Log.e("getTags", ret);
			JSONArray jarrTags = new JSONArray(ret);
			if (jarrTags != null && jarrTags.length() != 0) {
				list = new ArrayList<SbbsMeTag>();
				for (int i = 0; i < jarrTags.length(); i++) {
					list.add(SbbsMeTag.fromJson(jarrTags.getJSONObject(i)));
				}
			}
		} catch (Exception e) {

		}
		return list;
	}

	/**
	 * do NOT need login
	 * 
	 * @param userId
	 * @return
	 * @throws Exception
	 */
	public static SbbsMeBlock getLastBlockViaUser(String userId) {
		SbbsMeBlock block = null;
		try {
			String ret = HttpRequest.get(BASE_URL + "last_article/" + userId,
					"", HTTP.UTF_8);
			if (ret != null && (!ret.equals("null"))) {
				block = SbbsMeBlock.fromJson(new JSONObject(ret));
			}
		} catch (Exception e) {

		}
		return block;
	}

	/**
	 * do need login
	 * 
	 * @param fileName
	 * @return
	 */
	public static String uploadImage(String fileName) {
		List<BasicNameValuePair> files = new ArrayList<BasicNameValuePair>();
		files.add(new BasicNameValuePair("file", fileName));
		HttpRequestResponseData ret = HttpRequest.postFileWithHeader(BASE_URL
				+ "upload_image", new ArrayList<BasicNameValuePair>(), files,
				cookieData.cookie, HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("uploadImage", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @param fileName
	 * @return
	 */
	public static String deleteImage(String fileId) {
		Log.e("deleteImage", fileId);
		HttpRequestResponseData ret = HttpRequest.getWithHeader(BASE_URL
				+ "delete_image/" + fileId, "", cookieData.cookie, HTTP.UTF_8);
		String retStr = "";
		if (ret != null) {
			retStr = ret.data;
			Log.e("deleteImage", retStr);
		}
		return retStr;
	}

	/**
	 * do need login
	 * 
	 * @return
	 */
	public static List<SbbsMeImage> getImages() {
		HttpRequestResponseData ret = HttpRequest.getWithHeader(BASE_URL
				+ "get_images", "", cookieData.cookie, HTTP.UTF_8);
		List<SbbsMeImage> list = null;
		if (ret != null) {
			list = buildImageList(ret.data);
		}
		return list;
	}

	public static List<SbbsMeImage> getImages(int page, int pageSize) {
		HttpRequestResponseData ret = HttpRequest.getWithHeader(BASE_URL
				+ String.format("get_images/%d/%d", page, pageSize), "",
				cookieData.cookie, HTTP.UTF_8);
		List<SbbsMeImage> list = null;
		if (ret != null) {
			list = buildImageList(ret.data);
		}
		return list;
	}

	private static List<SbbsMeImage> buildImageList(String jsonRet) {
		List<SbbsMeImage> list = null;
		try {
			JSONArray jarrImage = new JSONArray(jsonRet);
			if (jarrImage != null && jarrImage.length() != 0) {
				list = new ArrayList<SbbsMeImage>();
				for (int i = 0; i < jarrImage.length(); i++) {
					list.add(SbbsMeImage.fromJson(jarrImage.getJSONObject(i)));
				}
			}
		} catch (Exception e) {

		}

		return list;
	}

	/**
	 * do need login
	 * 
	 * @param userId
	 * @param text
	 * @return
	 */
	public static String feedback(Context context, String userId, String email,
			String text) {
		List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
		params.add(new BasicNameValuePair("device", DeviceUtilsLite
				.getDeviceUniqueId(context)));
		params.add(new BasicNameValuePair("appver", String
				.valueOf(DeviceUtilsLite.getAppVersionCode(context))));
		params.add(new BasicNameValuePair("osver", String
				.valueOf(android.os.Build.VERSION.SDK_INT)));
		params.add(new BasicNameValuePair("text", text));
		params.add(new BasicNameValuePair("email", email));
		params.add(new BasicNameValuePair("userid", userId));
		String ret = HttpRequest.post(LOG_URL + "write_feedback.php", params,
				HTTP.UTF_8);
		return ret;
	}

	public static List<SbbsMeWeibo> getCredit(Context context) {
		List<SbbsMeWeibo> list = null;
		try {
			String ret = FileUtils.readAssetFile(context, "weibo.json");
			JSONArray jarrWeibo = new JSONArray(ret);
			if (jarrWeibo != null && jarrWeibo.length() != 0) {
				list = new ArrayList<SbbsMeWeibo>();
				for (int i = 0; i < jarrWeibo.length(); i++) {
					list.add(SbbsMeWeibo.fromJson(jarrWeibo.getJSONObject(i)));
				}
			}
		} catch (Exception e) {

		}

		return list;

	}

	public static void writeLogT(final Context context, final String action,
			final String detail) {

		new Thread(new Runnable() {

			@Override
			public void run() {
				List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
				params.add(new BasicNameValuePair("device", DeviceUtilsLite
						.getDeviceUniqueId(context)));
				params.add(new BasicNameValuePair("appver", String
						.valueOf(DeviceUtilsLite.getAppVersionCode(context))));
				params.add(new BasicNameValuePair("osver", String
						.valueOf(android.os.Build.VERSION.SDK_INT)));
				params.add(new BasicNameValuePair("email", AccountUtils
						.getBindedEmailAddress(context)));
				params.add(new BasicNameValuePair("action", action));
				params.add(new BasicNameValuePair("detail", detail));
				params.add(new BasicNameValuePair("actiontime", DateFormat
						.format("yyyyMMdd-hhmmss", System.currentTimeMillis())
						.toString()));
				String ret = HttpRequest.post(LOG_URL + "write_log.php",
						params, HTTP.UTF_8);
				Log.e("writeLogT", ret);
			}
		}).start();

	}
}
