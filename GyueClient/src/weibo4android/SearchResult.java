package weibo4android;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import weibo4android.http.Response;
import weibo4android.org.json.JSONArray;
import weibo4android.org.json.JSONException;
import weibo4android.org.json.JSONObject;
/**
 * 
 * @author sinaWeibo
 *
 */
public class SearchResult extends WeiboResponse implements java.io.Serializable {

	private static final long serialVersionUID = 8227371192527300467L;


	private Date createdAt;
	private long to_user_id;
	private String to_user;
	private String text;
	private String source;
	private long id;
	private long from_user_id;
	private String from_user;
	private String iso_language_code;
	private String profileImageUrl;
	public SearchResult(JSONObject json) throws WeiboException, JSONException{
		createdAt=parseDate(json.getString("created_at"), "EEE MMM dd HH:mm:ss z yyyy");
		to_user_id=json.getLong("to_user_id");
		to_user=json.getString("to_user");
		text=json.getString("text");
		source=json.getString("source");
		id=json.getLong("id");
		from_user_id=json.getLong("from_user_id");
		from_user=json.getString("from_user");
		iso_language_code=json.getString("iso_language_code");
		profileImageUrl=json.getString("profile_image_url");
	}
	public static List<SearchResult> constructResults(Response res)throws WeiboException{

		JSONObject json=res.asJSONObject();
		try {
			JSONArray list = json.getJSONArray("results");
			int size=list.length();
			List<SearchResult> rt=new ArrayList<SearchResult>(size);
			for(int i=0;i<size;i++){
				rt.add(new SearchResult(list.getJSONObject(i)));
			}
			return rt;


		}
		catch (JSONException je) {
			throw new WeiboException(je);
		}

	}


	public Date getCreatedAt() {
		return this.createdAt;
	}

	public long getToUserId() {
		return this.to_user_id;
	}

	public long getId() {
		return this.id;
	}

	public long getFromUserId() {
		return this.from_user_id;
	}

	public String getText() {
		return this.text;
	}
	public String getSource() {
		return this.source;
	}


	public String getFromUser() {
		return this.from_user;
	}

	public String getToUser() {
		return this.to_user;
	}

	public String getName() {
		return this.iso_language_code;
	}

	public URL getProfileImageURL() {
		try {
			return new URL(profileImageUrl);
		} catch (MalformedURLException ex) {
			return null;
		}
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
		+ ((from_user == null) ? 0 : from_user.hashCode());
		result = prime * result + (int) (from_user_id ^ (from_user_id >>> 32));
		result = prime * result + (int) (id ^ (id >>> 32));
		result = prime * result + ((to_user == null) ? 0 : to_user.hashCode());
		result = prime * result + (int) (to_user_id ^ (to_user_id >>> 32));
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
		SearchResult other = (SearchResult) obj;
		if (from_user == null) {
			if (other.from_user != null)
				return false;
		} else if (!from_user.equals(other.from_user))
			return false;
		if (from_user_id != other.from_user_id)
			return false;
		if (id != other.id)
			return false;
		if (to_user == null) {
			if (other.to_user != null)
				return false;
		} else if (!to_user.equals(other.to_user))
			return false;
		if (to_user_id != other.to_user_id)
			return false;
		return true;
	}


	public String toString() {

		return "Result{ " +to_user_id +
		"," +to_user+
		"," +text+
		"," +id+
		"," +from_user_id+
		"," +from_user+
		"," +iso_language_code+
		"," +source+
		"," +profileImageUrl+
		"," +createdAt+
		'}';

	}
}
