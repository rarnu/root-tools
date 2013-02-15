package com.rarnu.tools.root;

import android.app.Activity;
import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.text.Html;
import android.text.Html.ImageGetter;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

import com.rarnu.tools.root.utils.FileUtils;

public class EggActivity extends Activity implements OnClickListener {

	TextView tvDesc, tvWeibo, tvSource, tvCompileCode;
	Button btnClose;

	ImageGetter ig = new ImageGetter() {

		@Override
		public Drawable getDrawable(String source) {
			Drawable d = null;
			if (source.equals("weibo")) {
				d = getResources().getDrawable(R.drawable.weibo);
			} else if (source.equals("github")) {
				d = getResources().getDrawable(R.drawable.github_white);
			}
			if (d != null) {
				d.setBounds(0, 0, d.getIntrinsicWidth(), d.getIntrinsicHeight());
			}
			return d;
		}
	};

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_egg);

		tvDesc = (TextView) findViewById(R.id.tvDesc);
		tvWeibo = (TextView) findViewById(R.id.tvWeibo);
		tvSource = (TextView) findViewById(R.id.tvSource);
		tvCompileCode = (TextView) findViewById(R.id.tvCompileCode);
		btnClose = (Button) findViewById(R.id.btnClose);

		btnClose.setOnClickListener(this);
		tvWeibo.setOnClickListener(this);

		tvWeibo.setText(Html.fromHtml(getString(R.string.weibo_egg), ig, null));
		tvSource.setText(Html.fromHtml(getString(R.string.github_egg), ig, null));
		
		try {
			String compile = FileUtils.readAssetFile(this, "app_compile");
			tvCompileCode.setText(Html.fromHtml(compile));
		} catch (Exception e) {
		}
		
		try {
			String desc = FileUtils.readAssetFile(this, "app_dev");
			tvDesc.setText(Html.fromHtml(desc));
		} catch (Exception e) {
			
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvWeibo:
			Intent inWeibo = new Intent(Intent.ACTION_VIEW);
			inWeibo.setData(Uri.parse("http://weibo.com/rarnu"));
			startActivity(inWeibo);
			break;
		case R.id.btnClose:
			finish();
			break;
		}
	}
}
