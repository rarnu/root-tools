/*
Copyright (c) 2007-2009, Yusuke Yamamoto
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
 * Neither the name of the Yusuke Yamamoto nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Yusuke Yamamoto ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Yusuke Yamamoto BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package weibo4android;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import weibo4android.http.Response;
import weibo4android.org.json.JSONArray;
import weibo4android.org.json.JSONException;
import weibo4android.org.json.JSONObject;

/**
 * A data class representing Basic user information element
 */
public class User extends WeiboResponse implements java.io.Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3473349966713163765L;
	static final String[] POSSIBLE_ROOT_NAMES = new String[]{"user", "sender", "recipient", "retweeting_user"};
	private Weibo weibo;
	private long id;                      //用户id
	private String screenName;            //微博昵称
	private String name;                  //友好显示名称，如Bill Gates(此特性暂不支持)
	private int province;                 //省份编码（参考省份编码表）
	private int city;                     //城市编码（参考城市编码表）
	private String location;              //地址
	private String description;           //个人描述
	private String url;                   //用户博客地址
	private String profileImageUrl;       //自定义图像
	private String userDomain;            //用户个性化URL
	private String gender;                //性别,m--男，f--女,n--未知
	private int followersCount;           //粉丝数
	private int friendsCount;             //关注数
	private int statusesCount;            //微博数
	private int favouritesCount;          //收藏数
	private Date createdAt;               //创建时间
	private boolean following;            //保留字段,是否已关注(此特性暂不支持)
	private boolean verified;             //加V标示，是否微博认证用户
	private boolean geoEnabled;           //地理
	private boolean allowAllActMsg;       //保留字段（暂时不支持）

	private Status status = null;


	/*package*/User(Response res, Weibo weibo) throws WeiboException {
		super(res);
		Element elem = res.asDocument().getDocumentElement();
		init(res, elem, weibo);
	}

	/*package*/User(Response res, Element elem, Weibo weibo) throws WeiboException {
		super(res);
		init(res, elem, weibo);
	}
	/*package*/User(JSONObject json) throws WeiboException {
		super();
		init(json);
	}

	private void init(JSONObject json) throws WeiboException {
		if(json!=null){
			try {
				id = json.getLong("id");
				name = json.getString("name");
				screenName = json.getString("screen_name");
				location = json.getString("location");
				description = json.getString("description");
				profileImageUrl = json.getString("profile_image_url");
				url = json.getString("url");
				allowAllActMsg = json.getBoolean("allow_all_act_msg");
				followersCount = json.getInt("followers_count");
				friendsCount = json.getInt("friends_count");
				createdAt = parseDate(json.getString("created_at"), "EEE MMM dd HH:mm:ss z yyyy");
				favouritesCount = json.getInt("favourites_count");
				following = getBoolean("following", json);
				verified=getBoolean("verified", json);
				statusesCount = json.getInt("statuses_count");
				userDomain = json.getString("domain");
				gender = json.getString("gender");
				province = json.getInt("province");
				city = json.getInt("city");
				if (!json.isNull("status")) {
					setStatus(new Status(json.getJSONObject("status")));
				}
			} catch (JSONException jsone) {
				throw new WeiboException(jsone.getMessage() + ":" + json.toString(), jsone);
			}
		}
	}

	private void init(Response res,Element elem, Weibo weibo) throws WeiboException {
		this.weibo = weibo;
		ensureRootNodeNameIs(POSSIBLE_ROOT_NAMES, elem);
		id = getChildLong("id", elem);
		name = getChildText("name", elem);
		screenName = getChildText("screen_name", elem);
		location = getChildText("location", elem);
		description = getChildText("description", elem);
		profileImageUrl = getChildText("profile_image_url", elem);
		url = getChildText("url", elem);
		allowAllActMsg = getChildBoolean("allow_all_act_msg", elem);
		followersCount = getChildInt("followers_count", elem);
		friendsCount = getChildInt("friends_count", elem);
		createdAt = getChildDate("created_at", elem);
		favouritesCount = getChildInt("favourites_count", elem);
		following = getChildBoolean("following", elem);
		statusesCount = getChildInt("statuses_count", elem);
		geoEnabled = getChildBoolean("geo_enabled", elem);
		verified = getChildBoolean("verified", elem);
		userDomain = getChildText("domain", elem);
		gender = getChildText("gender", elem);
		province = getChildInt("province", elem);
		city = getChildInt("city", elem);
		status = new Status(res, (Element)elem.getElementsByTagName("status").item(0)
				, weibo);
	}

	/**
	 * Returns the id of the user
	 *
	 * @return the id of the user
	 */
	public long getId() {
		return id;
	}

	/**
	 * Returns the name of the user
	 *
	 * @return the name of the user
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns the screen name of the user
	 *
	 * @return the screen name of the user
	 */
	public String getScreenName() {
		return screenName;
	}

	/**
	 * Returns the location of the user
	 *
	 * @return the location of the user
	 */
	public String getLocation() {
		return location;
	}

	/**
	 * Returns the description of the user
	 *
	 * @return the description of the user
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Returns the profile image url of the user
	 *
	 * @return the profile image url of the user
	 */
	public URL getProfileImageURL() {
		try {
			return new URL(profileImageUrl);
		} catch (MalformedURLException ex) {
			return null;
		}
	}

	/**
	 * Returns the url of the user
	 *
	 * @return the url of the user
	 */
	public URL getURL() {
		try {
			return new URL(url);
		} catch (MalformedURLException ex) {
			return null;
		}
	}

	/**
	 * Test if the user status is protected
	 *
	 * @return true if the user status is protected
	 */
	public boolean isAllowAllActMsg() {
		return allowAllActMsg;
	}


	public String getUserDomain() {
		return userDomain;
	}

	/**
	 * Returns the number of followers
	 *
	 * @return the number of followers
	 * @since Weibo4J 1.2.1
	 */
	public int getFollowersCount() {
		return followersCount;
	}

	/**
	 * Returns the code of province
	 *
	 * @return the code of province
	 * @since Weibo4J 1.2.1
	 */
	public int getProvince() {
		return province;
	}

	/**
	 * Returns the code of city
	 *
	 * @return the code of city
	 * @since Weibo4J 1.2.1
	 */
	public int getCity() {
		return city;
	}

	public static List<User> constructUser(Response res)throws WeiboException{

		JSONObject json=res.asJSONObject();
		try {
			//			int next_cursor = json.getInt("next_cursor");
			//			int previous_cursor = json.getInt("previous_cursor");



			JSONArray list = json.getJSONArray("users");
			int size=list.length();
			List<User> users=new ArrayList<User>(size);
			for(int i=0;i<size;i++){
				users.add(new User(list.getJSONObject(i)));
			}
			return users;


		}
		catch (JSONException je) {
			throw new WeiboException(je);
		}

	}

	public static List<User> constructUsers(Response res, Weibo weibo) throws WeiboException {
		Document doc = res.asDocument();
		if (isRootNodeNilClasses(doc)) {
			return new ArrayList<User>(0);
		} else {
			try {
				ensureRootNodeNameIs("users", doc);
				//                NodeList list = doc.getDocumentElement().getElementsByTagName(
				//                        "user");
				//                int size = list.getLength();
				//                List<User> users = new ArrayList<User>(size);
				//                for (int i = 0; i < size; i++) {
				//                    users.add(new User(res, (Element) list.item(i), weibo));
				//                }

				//去除掉嵌套的bug
				NodeList list=doc.getDocumentElement().getChildNodes();
				List<User> users = new ArrayList<User>(list.getLength());
				Node node;
				for(int i=0;i<list.getLength();i++){
					node=list.item(i);
					if(node.getNodeName().equals("user")){
						users.add(new User(res, (Element) node, weibo));
					}
				}

				return users;
			} catch (WeiboException te) {
				if (isRootNodeNilClasses(doc)) {
					return new ArrayList<User>(0);
				} else {
					throw te;
				}
			}
		}
	}

	public static UserWapper constructWapperUsers(Response res, Weibo weibo) throws WeiboException {
		Document doc = res.asDocument();
		if (isRootNodeNilClasses(doc)) {
			return new UserWapper(new ArrayList<User>(0), 0, 0);
		} else {
			try {
				ensureRootNodeNameIs("users_list", doc);
				Element root = doc.getDocumentElement();
				NodeList user = root.getElementsByTagName("users");
				int length = user.getLength();
				if (length == 0) {
					return new UserWapper(new ArrayList<User>(0), 0, 0);
				}
				// 
				Element listsRoot = (Element) user.item(0);
				NodeList list=listsRoot.getChildNodes();
				List<User> users = new ArrayList<User>(list.getLength());
				Node node;
				for(int i=0;i<list.getLength();i++){
					node=list.item(i);
					if(node.getNodeName().equals("user")){
						users.add(new User(res, (Element) node, weibo));
					}
				}
				//
				long previousCursor = getChildLong("previous_curosr", root);
				long nextCursor = getChildLong("next_curosr", root);
				if (nextCursor == -1) { // 兼容不同标签名称
					nextCursor = getChildLong("nextCurosr", root);
				}
				return new UserWapper(users, previousCursor, nextCursor);
			} catch (WeiboException te) {
				if (isRootNodeNilClasses(doc)) {
					return new UserWapper(new ArrayList<User>(0), 0, 0);
				} else {
					throw te;
				}
			}
		}
	}

	public static List<User> constructUsers(Response res) throws WeiboException {
		try {
			JSONArray list = res.asJSONArray();
			int size = list.length();
			List<User> users = new ArrayList<User>(size);
			for (int i = 0; i < size; i++) {
				users.add(new User(list.getJSONObject(i)));
			}
			return users;
		} catch (JSONException jsone) {
			throw new WeiboException(jsone);
		} catch (WeiboException te) {
			throw te;
		}  
	}

	/**
	 * 
	 * @param res
	 * @return
	 * @throws WeiboException
	 */
	public static UserWapper constructWapperUsers(Response res) throws WeiboException {
		JSONObject jsonUsers = res.asJSONObject(); //asJSONArray();
		try {
			JSONArray user = jsonUsers.getJSONArray("users");
			int size = user.length();
			List<User> users = new ArrayList<User>(size);
			for (int i = 0; i < size; i++) {
				users.add(new User(user.getJSONObject(i)));
			}
			long previousCursor = jsonUsers.getLong("previous_curosr");
			long nextCursor = jsonUsers.getLong("next_cursor");
			if (nextCursor == -1) { // 兼容不同标签名称
				nextCursor = jsonUsers.getLong("nextCursor");
			}
			return new UserWapper(users, previousCursor, nextCursor);
		} catch (JSONException jsone) {
			throw new WeiboException(jsone);
		}
	}

	/**
	 * @param res 
	 * @return 
	 * @throws WeiboException
	 */
	static List<User> constructResult(Response res) throws WeiboException {
		JSONArray list = res.asJSONArray();
		try {
			int size = list.length();
			List<User> users = new ArrayList<User>(size);
			for (int i = 0; i < size; i++) {
				users.add(new User(list.getJSONObject(i)));
			}
			return users;
		} catch (JSONException e) {
		}
		return null;
	}

	public int getFriendsCount() {
		return friendsCount;
	}

	public Date getCreatedAt() {
		return createdAt;
	}

	public int getFavouritesCount() {
		return favouritesCount;
	}

	public String getGender() {
		return gender;
	}

	/**
	 *
	 * @deprecated
	 */
	public boolean isFollowing() {
		return following;
	}

	public int getStatusesCount() {
		return statusesCount;
	}

	/**
	 * @return the user is enabling geo location
	 * @since Weibo4J 1.2.1
	 */
	public boolean isGeoEnabled() {
		return geoEnabled;
	}

	/**
	 * @return returns true if the user is a verified celebrity
	 * @since Weibo4J 1.2.1
	 */
	public boolean isVerified() {
		return verified;
	}

	public void setStatus(Status status) {
		this.status = status;
	}

	public Status getStatus() {
		return status;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (id ^ (id >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		User other = (User) obj;
		if (id != other.id)
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "User{" +
		"weibo=" + weibo +
		", id=" + id +
		", name='" + name + '\'' +
		", screenName='" + screenName + '\'' +
		", location='" + location + '\'' +
		", description='" + description + '\'' +
		", profileImageUrl='" + profileImageUrl + '\'' +
		", province='" + province +'\'' +
		", city='" +city +'\''+
		", domain ='" + userDomain+ '\'' +
		", gender ='" + gender + '\'' +
		", url='" + url + '\'' +
		", allowAllActMsg=" + allowAllActMsg +
		", followersCount=" + followersCount +
		", friendsCount=" + friendsCount +
		", createdAt=" + createdAt +
		", favouritesCount=" + favouritesCount +
		", following=" + following +
		", statusesCount=" + statusesCount +
		", geoEnabled=" + geoEnabled +
		", verified=" + verified +
		", status=" + status +
		'}';
	}

}
