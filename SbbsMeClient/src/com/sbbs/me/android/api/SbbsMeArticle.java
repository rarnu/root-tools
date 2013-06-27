package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMeArticle {

	public String AuthorId;
	public String Body;
	public String Created_on;
	public String Format;
	public String Id;
	public double Order;
	public String ParentId;
	public int Public;
	public int RightBlockCount;
	public String Status;
	public String Subject;
	public String Type;
	public String Updated_on;
	public String _id;
	
	public static SbbsMeArticle fromJson(JSONObject json) throws Exception {
		SbbsMeArticle article = new SbbsMeArticle();
		
		article.AuthorId = json.getString("AuthorId");
		article.Body = json.getString("Body");
		article.Created_on = json.getString("Created_on");
		article.Format = json.getString("Format");
		article.Id = json.getString("Id");
		article.Order = json.getDouble("Order");
		article.ParentId = json.getString("ParentId");
		article.Public = json.getInt("Public");
		article.RightBlockCount = json.getInt("RightBlockCount");
		article.Status = json.getString("Status");
		article.Subject = json.getString("Subject");
		article.Type = json.getString("Type");
		article.Updated_on = json.getString("Updated_on");
		article._id = json.getString("_id");
		return article;
	}
}
