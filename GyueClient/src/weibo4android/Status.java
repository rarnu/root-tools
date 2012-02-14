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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import weibo4android.http.Response;
import weibo4android.org.json.JSONArray;
import weibo4android.org.json.JSONException;
import weibo4android.org.json.JSONObject;

/**
 * A data class representing one single status of a user.
 * @author Yusuke Yamamoto - yusuke at mac.com
 */
public class Status extends WeiboResponse implements java.io.Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -8795691786466526420L;

	private User user = null;
	
	private Date createdAt;             //status创建时间
	private long id;                    //status id
	private String text;                //微博内容
	private String source;              //微博来源
	private boolean isTruncated;        //保留字段
	private long inReplyToStatusId;
	private long inReplyToUserId;
	private boolean isFavorited;        //保留字段，未弃用
	private String inReplyToScreenName;
	private double latitude = -1;       //纬度
	private double longitude = -1;      //经度
	private String thumbnail_pic;       //微博内容中的图片的缩略地址
	private String bmiddle_pic;         //中型图片
	private String original_pic;        //原始图片
	private Status retweeted_status;    //转发的微博内容
	private String mid;                 //mid


	/*package*/Status(Response res, Weibo weibo) throws WeiboException {
		super(res);
		Element elem = res.asDocument().getDocumentElement();
		init(res, elem, weibo);
	}

	/*package*/Status(Response res, Element elem, Weibo weibo) throws
	WeiboException {
		super(res);
		init(res, elem, weibo);
	}

	Status(Response res)throws WeiboException{
		super(res);
		JSONObject json=res.asJSONObject();
		constructJson(json);
	}
	/* modify by Reilost add some field*/
	private void constructJson(JSONObject json) throws WeiboException {
		try {
			id = json.getLong("id");
			text = json.getString("text");
			source = json.getString("source");
			createdAt = parseDate(json.getString("created_at"), "EEE MMM dd HH:mm:ss z yyyy");
			inReplyToStatusId = getLong("in_reply_to_status_id", json);
			inReplyToUserId = getLong("in_reply_to_user_id", json);
			isFavorited = getBoolean("favorited", json);
			thumbnail_pic = json.getString("thumbnail_pic");
			bmiddle_pic = json.getString("bmiddle_pic");
			original_pic = json.getString("original_pic");
			if(!json.isNull("user"))
				user = new User(json.getJSONObject("user"));
			inReplyToScreenName=json.getString("inReplyToScreenName");
			if(!json.isNull("retweeted_status")){
				retweeted_status= new Status(json.getJSONObject("retweeted_status"));
			}
			
			mid=json.getString("mid");
			String geo= json.getString("geo");
			if(geo!=null &&!"".equals(geo) &&!"null".equals(geo)){
				getGeoInfo(geo);
			}
		} catch (JSONException je) {
			throw new WeiboException(je.getMessage() + ":" + json.toString(), je);
		}
	}

	private void getGeoInfo(String geo) {
		StringBuffer value= new StringBuffer();
		for(char c:geo.toCharArray()){
			if(c>45&&c<58){
				value.append(c);
			}
			if(c==44){
				if(value.length()>0){
					latitude=Double.parseDouble(value.toString());
					value.delete(0, value.length());
				}
			}
		}
		longitude=Double.parseDouble(value.toString());
	}


	public Status(JSONObject json)throws WeiboException, JSONException{
		constructJson(json);
	}
	public Status(String str) throws WeiboException, JSONException {
		// StatusStream uses this constructor
		super();
		JSONObject json = new JSONObject(str);
		constructJson(json);
	}

	private void init(Response res, Element elem, Weibo weibo) throws
	WeiboException {
		ensureRootNodeNameIs("status", elem);
		user = new User(res, (Element) elem.getElementsByTagName("user").item(0)
				, weibo);
		id = getChildLong("id", elem);
		text = getChildText("text", elem);
		source = getChildText("source", elem);
		createdAt = getChildDate("created_at", elem);
		isTruncated = getChildBoolean("truncated", elem);
		inReplyToStatusId = getChildLong("in_reply_to_status_id", elem);
		inReplyToUserId = getChildLong("in_reply_to_user_id", elem);
		isFavorited = getChildBoolean("favorited", elem);
		inReplyToScreenName = getChildText("in_reply_to_screen_name", elem);
		NodeList georssPoint = elem.getElementsByTagName("georss:point");
		if(1 == georssPoint.getLength()){
			String[] point = georssPoint.item(0).getFirstChild().getNodeValue().split(" ");
			if(!"null".equals(point[0]))
				latitude = Double.parseDouble(point[0]);
			if(!"null".equals(point[1]))
				longitude = Double.parseDouble(point[1]);
		}
		NodeList retweetDetailsNode = elem.getElementsByTagName("retweet_details");
		if(1 == retweetDetailsNode.getLength()){
			retweeted_status= new Status(res,(Element)retweetDetailsNode.item(0),weibo);
		}
	}

	/**
	 * Return the created_at
	 *
	 * @return created_at
	 * @since Weibo4J 1.1.0
	 */

	public Date getCreatedAt() {
		return this.createdAt;
	}

	/**
	 * Returns the id of the status
	 *
	 * @return the id
	 */
	public long getId() {
		return this.id;
	}

	/**
	 * Returns the text of the status
	 *
	 * @return the text
	 */
	public String getText() {
		return this.text;
	}

	/**
	 * Returns the source
	 *
	 * @return the source
	 * @since Weibo4J 1.2.1
	 */
	public String getSource() {
		return this.source;
	}


	/**
	 * Test if the status is truncated
	 *
	 * @return true if truncated
	 * @since Weibo4J 1.2.1
	 */
	public boolean isTruncated() {
		return isTruncated;
	}

	/**
	 * Returns the in_reply_tostatus_id
	 *
	 * @return the in_reply_tostatus_id
	 * @since Weibo4J 1.2.1
	 */
	public long getInReplyToStatusId() {
		return inReplyToStatusId;
	}

	/**
	 * Returns the in_reply_user_id
	 *
	 * @return the in_reply_tostatus_id
	 * @since Weibo4J 1.2.1
	 */
	public long getInReplyToUserId() {
		return inReplyToUserId;
	}

	/**
	 * Returns the in_reply_to_screen_name
	 *
	 * @return the in_in_reply_to_screen_name
	 * @since Weibo4J 1.2.1
	 */
	public String getInReplyToScreenName() {
		return inReplyToScreenName;
	}

	/**
	 * returns The location's latitude that this tweet refers to.
	 *
	 * @since Weibo4J 1.2.1
	 */
	public double getLatitude() {
		return latitude;
	}

	/**
	 * returns The location's longitude that this tweet refers to.
	 *
	 * @since Weibo4J 1.2.1
	 */
	public double getLongitude() {
		return longitude;
	}

	/**
	 * Test if the status is favorited
	 *
	 * @return true if favorited
	 * @since Weibo4J 1.2.1
	 */
	public boolean isFavorited() {
		return isFavorited;
	}

	public String getThumbnail_pic() {
		return thumbnail_pic;
	}

	public String getBmiddle_pic() {
		return bmiddle_pic;
	}

	public String getOriginal_pic() {
		return original_pic;
	}


	/**
	 * Return the user
	 *
	 * @return the user
	 */
	public User getUser() {
		return user;
	}

	/**
	 *
	 * @since Weibo4J 1.2.1
	 */
	public boolean isRetweet(){
		return null != retweeted_status;
	}

	public Status getRetweeted_status() {
		return retweeted_status;
	}

	public String getMid() {
		return mid;
	}

	/*package*/
	static List<Status> constructStatuses(Response res,
			Weibo weibo) throws WeiboException {

		Document doc = res.asDocument();
		if (isRootNodeNilClasses(doc)) {
			return new ArrayList<Status>(0);
		} else {
			try {
				ensureRootNodeNameIs("statuses", doc);
				NodeList list = doc.getDocumentElement().getElementsByTagName(
				"status");
				int size = list.getLength();
				List<Status> statuses = new ArrayList<Status>(size);
				for (int i = 0; i < size; i++) {
					Element status = (Element) list.item(i);
					statuses.add(new Status(res, status, weibo));
				}
				return statuses;
			} catch (WeiboException te) {
				ensureRootNodeNameIs("nil-classes", doc);
				return new ArrayList<Status>(0);
			}
		}

	}

	/*modify by sycheng add json call method*/
	/*package*/
	static List<Status> constructStatuses(Response res) throws WeiboException {
		try {
			JSONArray list = res.asJSONArray();
			int size = list.length();
			List<Status> statuses = new ArrayList<Status>(size);

			for (int i = 0; i < size; i++) {
				statuses.add(new Status(list.getJSONObject(i)));
			}
			return statuses;
		} catch (JSONException jsone) {
			throw new WeiboException(jsone);
		} catch (WeiboException te) {
			throw te;
		}

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
		Status other = (Status) obj;
		if (id != other.id)
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "Status [createdAt=" + createdAt + ", id=" + id + ", text="
		+ text + ", source=" + source + ", isTruncated=" + isTruncated
		+ ", inReplyToStatusId=" + inReplyToStatusId
		+ ", inReplyToUserId=" + inReplyToUserId + ", isFavorited="
		+ isFavorited + ", inReplyToScreenName=" + inReplyToScreenName
		+ ", latitude=" + latitude + ", longitude=" + longitude
		+ ", thumbnail_pic=" + thumbnail_pic + ", bmiddle_pic="
		+ bmiddle_pic + ", original_pic=" + original_pic
		+ ",  mid=" + mid + ", user=" + user 
		+", retweeted_status="+(retweeted_status==null?"null":retweeted_status.toString())+ 
		"]";
	}


}
