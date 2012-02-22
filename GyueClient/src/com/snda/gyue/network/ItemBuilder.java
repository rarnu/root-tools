package com.snda.gyue.network;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import android.content.Context;

import com.snda.gyue.GyueConsts;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.utils.FileUtils;
import com.snda.gyue.utils.XmlUtils;

// http://www.gyue.cn/index.php?m=content&c=rss&rssid=54&page=1&size=20

public class ItemBuilder {

	public static List<ArticleItem> xmlToItems(Context context, int rssid, String xml, boolean local, boolean saveMain)
			throws Exception {

		List<ArticleItem> result = null;
		FileUtils.mkdir(GyueConsts.GYUE_DIR);
		String localFilePath = GyueConsts.GYUE_DIR + String.format("a%d.xml", rssid);

		if (!saveMain) {
			localFilePath += ".tmp";
		}

		File fXml = new File(localFilePath);

		if (!local) {
			xml = xml.replace("<?xml version=\"1.0\" encoding=\"gbk\"?>", "<?xml version=\"1.0\" encoding=\"utf-8\"?>");
			FileUtils.rewriteFile(fXml, xml);
		}

		if (!fXml.exists()) {
			return null;
		}

		XmlUtils util = new XmlUtils();
		util.initialize();

		if (util.loadFile(context, fXml)) {
			NodeList nodeList = ((Element) util.getRoot().getElementsByTagName("channel").item(0))
					.getElementsByTagName("item");
			Element ele = null;
			if (nodeList != null && nodeList.getLength() > 0) {
				result = new ArrayList<ArticleItem>();
				for (int i = 0; i < nodeList.getLength(); i++) {
					ele = (Element) nodeList.item(i);
					ArticleItem item = new ArticleItem();
					item.setUid(util.getNodeValue((Element) ele.getElementsByTagName("uid").item(0)));
					item.setTitle(util.getNodeValue((Element) ele.getElementsByTagName("title").item(0)));
					item.setLink(util.getNodeValue((Element) ele.getElementsByTagName("link").item(0)));
					item.setDescription(util.getNodeValue((Element) ele.getElementsByTagName("description").item(0)));
					item.setDate(util.getNodeValue((Element) ele.getElementsByTagName("pubDate").item(0)));
					item.setAuthor(util.getNodeValue((Element) ele.getElementsByTagName("author").item(0)));
					item.setComment(util.getNodeValue((Element) ele.getElementsByTagName("comments").item(0)));
					item.setArticleImageUrl(findImage(item.getDescription(), item.getComment()));
					result.add(item);
				}
			}
		}

		util.finalize();
		
		FakeClick.doFakeClickAll(result);
		
		return result;
	}

	private static String findImage(String desc, String comment) {
		String url = desc + comment;
		String result = "";
		if (url.contains("<img")) {
			int p = url.indexOf("<img");
			if (p != -1) {

				for (int i = p; i < url.length(); i++) {
					if (url.charAt(i) == '>') {
						result += url.charAt(i);
						break;

					}
					result += url.charAt(i);
				}
			}
		}
		return result;
	}

}
