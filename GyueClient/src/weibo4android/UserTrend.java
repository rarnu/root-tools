package weibo4android;

import java.util.ArrayList;
import java.util.List;

import weibo4android.http.Response;
import weibo4android.org.json.JSONArray;
import weibo4android.org.json.JSONException;
import weibo4android.org.json.JSONObject;

/**
 * 话题
 *
 * @author SinaWeibo
 * @since Weibo4J 1.2.0
 */
public class UserTrend extends WeiboResponse{
    private String num;
    private String hotword = null;
    private String trend_id = null;
    private static final long serialVersionUID = 1925956704460743946L;
    
    
    
    
	public UserTrend() {
		super();
	}
	public UserTrend(Response res) throws WeiboException {
		super(res);
		JSONObject json = res.asJSONObject();
		try {
			num = json.getString("num");
			hotword = json.getString("hotword");
			trend_id = json.getString("trend_id");
			if( json.getString("topicid")!=null)
				trend_id = json.getString("topicid");
		} catch (JSONException je) {
			throw new WeiboException(je.getMessage() + ":" + json.toString(),
					je);
		}
	}
	public UserTrend(JSONObject json) throws WeiboException {
		try {
			num = json.getString("num");
			hotword = json.getString("hotword");
			trend_id = json.getString("trend_id");
		} catch (JSONException je) {
			throw new WeiboException(je.getMessage() + ":" + json.toString(),
					je);
		}
	}
	
	static List<UserTrend> constructTrendList(Response res) throws WeiboException {
	   	 try {
	            JSONArray list = res.asJSONArray();
	            int size = list.length();
	            List<UserTrend> trends = new ArrayList<UserTrend>(size);
	            for (int i = 0; i < size; i++) {
	            	trends.add(new UserTrend(list.getJSONObject(i)));
	            }
	            return trends;
	        } catch (JSONException jsone) {
	            throw new WeiboException(jsone);
	        } catch (WeiboException te) {
	            throw te;
	        }

	   }
	public String getNum() {
		return num;
	}
	public void setNum(String num) {
		this.num = num;
	}
	public String getHotword() {
		return hotword;
	}
	public void setHotword(String hotword) {
		this.hotword = hotword;
	}
	public String getTrend_id() {
		return trend_id;
	}
	public void setTrend_id(String trend_id) {
		this.trend_id = trend_id;
	}
	@Override
	public String toString() {
		return "Trend [num=" + num + ", hotword=" + hotword + ", trend_id="
				+ trend_id + "]";
	}
    
    
}