package com.rarnu.zoe.loving;

import android.app.Activity;
import android.content.res.Configuration;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

import com.rarnu.zoe.loving.comp.ScrollLayout;
import com.rarnu.zoe.loving.comp.ScrollLayout.OnScreenChangeListener;

public class GalleryActivity extends Activity implements OnScreenChangeListener {

	ScrollLayout gf;
	TextView tvIndex;
	int page = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		initData(0);

	}

	private void initData(int index) {
		page = index;
		setContentView(R.layout.activity_gallery);
		gf = (ScrollLayout) findViewById(R.id.gf);
		tvIndex = (TextView) findViewById(R.id.tvIndex);

		for (int i = 0; i < 10; i++) {
			ImageView img = new ImageView(this);
			img.setLayoutParams(new RelativeLayout.LayoutParams(
					LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT));
			img.setAdjustViewBounds(true);
			img.setScaleType(ScaleType.CENTER_INSIDE);
			img.setImageDrawable(getResources().getDrawable(R.drawable.test));
			gf.addView(img);
		}

		gf.setOnScreenChangeListener(this);
		gf.setToScreen(index);
		tvIndex.setText(String.format("%d / %d", index + 1, 10));
	}

	@Override
	public void onScreenChange(int screen) {
		page = screen;
		tvIndex.setText(String.format("%d / %d", screen + 1, 10));

	}

	@Override
	public void onConfigurationChanged(Configuration newConfig) {
		super.onConfigurationChanged(newConfig);
		initData(page);
	}
}
