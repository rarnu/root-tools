package com.snda.gyue.classes;


public class ArticleItem {

	private String title;
	private String link;
	private String date;
	private String author;
	private String comment;
	private String description;
	private String articleImageUrl;
	private String articleImageLocalFileName;
	private String downloadApkUrl;

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
		extractDownloadUrl(comment);
		comment = comment.replaceAll("(?is)<table.*?</table>", "");
		comment = comment.replaceAll("<p>", "").replaceAll("</p>", "<br />");
		comment = comment.replaceAll("<center>", "").replaceAll("</center>", "");
		comment = comment.replaceAll("\\[page\\]", "").replaceAll("\\[/page\\]", "");
		comment = comment.replaceAll("\t", "");
		this.comment = comment.trim();
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		description = description.replaceAll("(?is)<table.*?</table>", "");
		description = description.replaceAll("<img[^>]*?((>.*?</img>)|(/>))", "");
		description = description.replaceAll("<br[^>]*?((>.*?</br>)|(/>))", "");
		description = description.replaceAll("<p[^>]*?((>.*?</p>)|(/>))", "");
		this.description = description.trim();
	}

	public String getArticleImageUrl() {
		return articleImageUrl;
	}

	public void setArticleImageUrl(String articleImageUrl) {
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

	public String getDownloadApkUrl() {
		return downloadApkUrl;
	}

	public void setDownloadApkUrl(String downloadApkUrl) {
		this.downloadApkUrl = downloadApkUrl;
	}
	
	private void extractDownloadUrl(String comment) {
		if (comment.indexOf("http://static.ggg.cn/apks") < 0) {
			downloadApkUrl = "";
			return;
		}
		String s = comment.substring(comment.indexOf("http://static.ggg.cn/apks"));
		String ret = "";
		for (int i=0; i<s.length(); i++) {
			if (s.charAt(i) != '\"') {
				ret += s.charAt(i);
			} else {
				break;
			}
		}
		downloadApkUrl = ret;
	}
}
