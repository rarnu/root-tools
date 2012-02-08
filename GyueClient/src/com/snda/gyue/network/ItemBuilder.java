package com.snda.gyue.network;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import android.content.Context;
import android.util.Log;

import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.utils.FileUtils;
import com.snda.gyue.utils.XmlUtils;

// http://www.gyue.cn/index.php?m=content&c=rss&rssid=54&page=1&size=20

public class ItemBuilder {

	private static final String TEMP_DIR = "/sdcard/.gyue/";

	public static List<ArticleItem> xmlToItems(Context context, int rssid, String xml, boolean local) throws Exception {

		List<ArticleItem> result = null;
		FileUtils.mkdir(TEMP_DIR);
		String localFilePath = TEMP_DIR + String.format("a%d.xml", rssid);

		File fXml = new File(localFilePath);

		if (!local) {
			xml = xml.replace("<?xml version=\"1.0\" encoding=\"gbk\"?>", "<?xml version=\"1.0\" encoding=\"utf-8\"?>");
			FileUtils.rewriteFile(fXml, xml);
		} else {
			if (xml == null || xml.equals("")) {
				// 
			}
		}

		if (!fXml.exists()) {
			return null;
		}

		XmlUtils util = new XmlUtils();
		util.initialize();

		if (util.loadFile(context, fXml)) {
			NodeList nodeList = ((Element) util.getRoot().getElementsByTagName("channel").item(0)).getElementsByTagName("item");
			Element ele = null;
			if (nodeList != null && nodeList.getLength() > 0) {
				result = new ArrayList<ArticleItem>();
				for (int i = 0; i < nodeList.getLength(); i++) {
					ele = (Element) nodeList.item(i);
					ArticleItem item =new ArticleItem();
					item.title = util.getNodeValue((Element)ele.getElementsByTagName("title").item(0));
					item.link = util.getNodeValue((Element) ele.getElementsByTagName("link").item(0));
					item.description = util.getNodeValue((Element) ele.getElementsByTagName("description").item(0));
					item.date = util.getNodeValue((Element) ele.getElementsByTagName("pubDate").item(0));
					item.author = util.getNodeValue((Element) ele.getElementsByTagName("author").item(0));
					item.comment = util.getNodeValue((Element) ele.getElementsByTagName("comments").item(0));
					Log.e("ITEM_BUILDER", item.title);
					result.add(item);
				}
			}
		}

		util.finalize();
		return result;
	}

}
