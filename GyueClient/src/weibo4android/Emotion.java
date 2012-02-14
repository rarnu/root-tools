package weibo4android;

import java.util.ArrayList;
import java.util.List;

import weibo4android.http.Response;
import weibo4android.org.json.JSONArray;
import weibo4android.org.json.JSONException;
import weibo4android.org.json.JSONObject;

/**
 * @author sinaWeibo
 *
 */
public class Emotion extends WeiboResponse{
	private static final long serialVersionUID = -4096813631691846494L;

	private String phrase;

	private String type;

	private String url;

	private boolean is_hot;

	private boolean is_common;

	private int order_number;

	private String category;

	public Emotion(Response res) throws WeiboException {
		super(res);
		JSONObject json = res.asJSONObject();
		try {
			phrase = json.getString("phrase");
			type = json.getString("type");
			url = json.getString("url");
			is_hot= json.getBoolean("is_hot");
			order_number = json.getInt("order_number");
			category = json.getString("category");
			is_common = json.getBoolean("is_common");
		} catch (JSONException je) {
			throw new WeiboException(je.getMessage() + ":" + json.toString(),
					je);
		}
	}
	public Emotion(JSONObject json) throws WeiboException {
		try {
			phrase = json.getString("phrase");
			type = json.getString("type");
			url = json.getString("url");
			is_hot= json.getBoolean("is_hot");
			order_number = json.getInt("order_number");
			category = json.getString("category");
			is_common = json.getBoolean("is_common");
		} catch (JSONException je) {
			throw new WeiboException(je.getMessage() + ":" + json.toString(),
					je);
		}
	}
    static List<Emotion> constructEmotions(Response res) throws WeiboException {
   	 try {
            JSONArray list = res.asJSONArray();
            int size = list.length();
            List<Emotion> emotions = new ArrayList<Emotion>(size);
            for (int i = 0; i < size; i++) {
            	emotions.add(new Emotion(list.getJSONObject(i)));
            }
            return emotions;
        } catch (JSONException jsone) {
            throw new WeiboException(jsone);
        } catch (WeiboException te) {
            throw te;
        }

   }
	public Emotion() {
		super();
	}




	public String getPhrase() {
		return phrase;
	}

	public void setPhrase(String phrase) {
		this.phrase = phrase;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public boolean isIs_hot() {
		return is_hot;
	}

	public void setIs_hot(boolean isHot) {
		is_hot = isHot;
	}

	public boolean isIs_common() {
		return is_common;
	}

	public void setIs_common(boolean isCommon) {
		is_common = isCommon;
	}

	public int getOrder_number() {
		return order_number;
	}

	public void setOrder_number(int orderNumber) {
		order_number = orderNumber;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}
	@Override
	public String toString() {
	    return "Emotion [phrase=" + phrase + ", type=" + type + ", url="
		    + url + ", is_hot=" + is_hot + ", is_common=" + is_common
		    + ", order_number=" + order_number + ", category="
		    + category + "]";
	}

}
