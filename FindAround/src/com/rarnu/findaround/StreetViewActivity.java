package com.rarnu.findaround;

import org.apache.http.protocol.HTTP;

import android.os.Bundle;

import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.comp.LavenderView;

public class StreetViewActivity extends BaseActivity {

	LavenderView lvStreet;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.street);
		lvStreet = (LavenderView) findViewById(R.id.lvStreet);
		String data = buildMapPage();
		lvStreet.loadDataWithBaseURL("http://map.baidu.com", data, "text/html",
				HTTP.UTF_8, "");
	}

	private String buildMapPage() {

		// int width = UIUtils.pxToDip(UIUtils.getWidth());
		// int height = UIUtils.pxToDip(UIUtils.getHeight());
		
		int width = 320;
		int height = 480;

		String html = "<!DOCTYPE html>";
		html += "<html>";
		html += "<head>";
		html += "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />";
		html += "<script type=\"text/javascript\" src=\"http://api.map.baidu.com/api?v=1.3\"></script>";
		html += "</head>";
		html += "<body width=\"320\" height=\"480\">";
		html += "<div style=\"width:" + String.valueOf(width) + "px;height:"
				+ String.valueOf(height)
				+ "px;border:1px solid gray\" id=\"container\"></div>";
		html += "</body>";
		html += "</html>";
		html += "<script type=\"text/javascript\">";
		html += "var map = new BMap.Map(\"container\", {mapType:BMAP_PERSPECTIVE_MAP});";
		html += "var point = new BMap.Point(121.510788,31.243839);";
		html += "map.setCurrentCity(\"上海\");";
		html += "map.centerAndZoom(point,16);";
		html += "</script>";
		return html;

	}
}
