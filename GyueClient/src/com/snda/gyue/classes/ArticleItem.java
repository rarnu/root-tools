package com.snda.gyue.classes;

import android.util.Log;

public class ArticleItem {

	private String title;
	private String link;
	private String date;
	private String author;
	private String comment;
	private String description;
	private String articleImageUrl;
	private String articleImageLocalFileName;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title.trim();
	}

	public String getLink() {
		return link;
	}

	public void setLink(String link) {
		this.link = link.trim();
	}

	public String getDate() {
		return date;
	}

	public void setDate(String date) {
		date = date.substring(5);
		this.date = date.trim();
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author.trim();
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment.trim();
	}

	public String getDescription() {
		Log.e("GET DESC", description);
		return description;
	}

	public void setDescription(String description) {
		description = description.replaceAll("<img[^>]*?((>.*?</img>)|(/>))", "");
		description = description.replaceAll("<br[^>]*?((>.*?</br>)|(/>))", "");
		description = description.replaceAll("<p[^>]*?((>.*?</p>)|(/>))", "");
		this.description = description.trim();
	}

	public String getArticleImageUrl() {
		return articleImageUrl;
	}

	public void setArticleImageUrl(String articleImageUrl) {

		// src="http://www.gyue.cn/uploadfile/2012/0208/20120208093319687.jpg"
		String local = "";

		int p = articleImageUrl.indexOf("src=");
		if (p != -1) {
			for (int i = p + 5; i < articleImageUrl.length(); i++) {
				if (articleImageUrl.charAt(i) == '\"') {
					break;
				}
				local += articleImageUrl.charAt(i);
			}
		}

		this.articleImageUrl = local;
		setArticleImageLocalFileName(local);
	}

	public String getArticleImageLocalFileName() {
		return articleImageLocalFileName;
	}

	public void setArticleImageLocalFileName(String articleImageLocalFileName) {

		String local = "";
		for (int i = articleImageLocalFileName.length() - 1; i >= 0; i--) {
			if (articleImageLocalFileName.charAt(i) == '/') {
				break;
			}
			local = articleImageLocalFileName.charAt(i) + local;
		}

		this.articleImageLocalFileName = local;
	}
}
