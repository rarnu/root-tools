package weibo4android;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import weibo4android.http.Response;
import weibo4android.org.json.JSONArray;
import weibo4android.org.json.JSONException;
import weibo4android.org.json.JSONObject;

/**
 * @author sinaWeibo
 *
 */
public class Tag extends WeiboResponse implements java.io.Serializable{

	private static final long serialVersionUID = 2177657076940291492L;

	private String id;

	private String value;

	public Tag(Response res,Element elem) throws WeiboException{
		ensureRootNodeNameIs("tag", elem);
		id = getChildText("id",elem);
		value = getChildText("value",elem);
	
	
	}
	public Tag(Response res ,Element elem,Weibo weibo,String str) throws WeiboException{
		ensureRootNodeNameIs("tagid", elem);
		id=elem.getNodeName();
		value=elem.getNodeValue();
	}
	public Tag(Response res,Element elem,Weibo weibo) throws WeiboException{
		ensureRootNodeNameIs("tagid", elem);
		id=elem.getNodeName();
		value=elem.getNodeValue();
	}

	

	public Tag(JSONObject json) throws WeiboException, JSONException {
		if (!(json.getString("id")==null||json.getString("id").length()==0)) {
			id = json.getString("id");
		} else if (!(json.getString("tagid")==null||json.getString("tagid").length()==0)) {
			id = json.getString("tagid");
		}
		if (!(json.getString("value")==null||json.getString("value").length()==0))
			value = json.getString("value");

	}

	public static List<Tag> constructTags(Response res,Weibo weibo) throws WeiboException {
		Document doc = res.asDocument();
		if (isRootNodeNilClasses(doc)) {
			return new ArrayList<Tag>(0);
		} else {
			try {
				
				ensureRootNodeNameIs("tags", doc);
				NodeList list = doc.getDocumentElement().getElementsByTagName(
				"tag");
				int size = list.getLength();
				List<Tag> tags = new ArrayList<Tag>(size);
				for (int i = 0; i < size; i++) {
					tags.add(new Tag(res, (Element) list.item(i)));
				}
				return tags;
			} catch (WeiboException te) {
				ensureRootNodeNameIs("nil-classes", doc);
				return new ArrayList<Tag>(0);
			}
		}
	}
    public static List<Tag> createTags(Response res,Weibo weibo) throws WeiboException{
		Document doc=res.asDocument();
		if(isRootNodeNilClasses(doc)){
			return new ArrayList<Tag>(0);
		}else{
			try{
				ensureRootNodeNameIs("tagids",doc);
				NodeList list = doc.getDocumentElement().getElementsByTagName(
				"tagid");
				int size = list.getLength();
				List<Tag> tags = new ArrayList<Tag>(size);
				for (int i = 0; i < size; i++) {
					tags.add(new Tag(res, (Element) list.item(i),null));
				}
				return tags;
			} catch (WeiboException te) {
				ensureRootNodeNameIs("nil-classes", doc);
				return new ArrayList<Tag>(0);
			}
			
		}
    
    	
    }

    public static List<Tag> destroyTags(Response res,Weibo weibo) throws WeiboException{
		Document doc=res.asDocument();
		if(isRootNodeNilClasses(doc)){
			return new ArrayList<Tag>(0);
		}else{
			try{
				ensureRootNodeNameIs("tags",doc);
				NodeList list = doc.getDocumentElement().getElementsByTagName(
				"tagid");
				int size = list.getLength();
				List<Tag> tags = new ArrayList<Tag>(size);
				for (int i = 0; i < size; i++) {
					tags.add(new Tag(res, (Element)list.item(i),null,null));
				}
				return tags;
			} catch (WeiboException te) {
				ensureRootNodeNameIs("nil-classes", doc);
				return new ArrayList<Tag>(0);
			}
			
		}
    
    	
    }



	static List<Tag> constructTags(Response res) throws WeiboException {
		try {
			JSONArray list = res.asJSONArray();
			int size = list.length();
			List<Tag>tags  = new ArrayList<Tag>(size);
			for (int i = 0; i < size; i++) {
				tags.add(new Tag(list.getJSONObject(i)));
			}
			return tags;
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
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
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
		Tag other = (Tag) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}



	/**
	 * @return the value
	 */
	public String getValue() {
		return value;
	}

	public String toString() {

		return "tags{ " +id +
		"," +value+
		'}';

	}


}
