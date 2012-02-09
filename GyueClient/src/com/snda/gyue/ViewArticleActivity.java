package com.snda.gyue;

import org.xml.sax.XMLReader;

import android.app.Activity;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.Editable;
import android.text.Html;
import android.text.Html.ImageGetter;
import android.text.Html.TagHandler;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;

public class ViewArticleActivity extends Activity implements OnClickListener {

	Button btnBack;
	ProgressBar pbRefreshing;
	TextView tvArticle;
	TextView tvTitle, tvDate;
	
	ImageGetter iGetter;
	TagHandler iTag;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.view_article);

		btnBack = (Button) findViewById(R.id.btnBack);
		pbRefreshing = (ProgressBar) findViewById(R.id.pbRefreshing);
		tvArticle = (TextView) findViewById(R.id.tvArticle);
		tvTitle = (TextView) findViewById(R.id.tvTitle);
		tvDate = (TextView) findViewById(R.id.tvDate);

		btnBack.setOnClickListener(this);
		
		tvTitle.setText(GlobalInstance.currentArticle.getTitle());
		tvDate.setText(GlobalInstance.currentArticle.getDate());
		
		iGetter = new ImageGetter() {
			
			@Override
			public Drawable getDrawable(String source) {
				// TODO Auto-generated method stub
				return null;
			}
		};
		
		iTag = new TagHandler() {
			
			@Override
			public void handleTag(boolean opening, String tag, Editable output, XMLReader xmlReader) {
				// TODO Auto-generated method stub
				
			}
		};
		
		tvArticle.setText(Html.fromHtml(GlobalInstance.currentArticle.getComment(), iGetter, iTag));
		
		
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnBack:
			finish();
			break;
		}

	}
}
