package com.rarnu.zoe.loving;

import android.app.Activity;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
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
	int day = 0;

	int[] bpImgs = new int[] { R.drawable.bp1, R.drawable.bp2, R.drawable.bp3,
			R.drawable.bp4, R.drawable.bp5, R.drawable.bp6, R.drawable.bp7,
			R.drawable.bp8, R.drawable.bp9, R.drawable.bp10, R.drawable.bp11,
			R.drawable.bp12, R.drawable.bp13, R.drawable.bp14, R.drawable.bp15,
			R.drawable.bp16, R.drawable.bp17, R.drawable.bp18, R.drawable.bp19,
			R.drawable.bp20, R.drawable.bp21 };

	ImageView[] ivs = new ImageView[21];
	Bitmap[] bps = new Bitmap[21];

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		int index = getIntent().getIntExtra("index", 0);
		initData(index);
	}

	private void initData(int index) {
		page = index;
		setContentView(R.layout.activity_gallery);
		gf = (ScrollLayout) findViewById(R.id.gf);
		tvIndex = (TextView) findViewById(R.id.tvIndex);

		day = Global.database.getDay();
		day = 21;

		for (int i = 0; i < day; i++) {
			ivs[i] = new ImageView(this);
			ivs[i].setLayoutParams(new RelativeLayout.LayoutParams(
					LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT));
			ivs[i].setAdjustViewBounds(true);
			ivs[i].setScaleType(ScaleType.CENTER_INSIDE);
			gf.addView(ivs[i]);
		}

		gf.setOnScreenChangeListener(this);
		gf.setToScreen(index);
		loadImages(index);
		tvIndex.setText(String.format("%d / %d", index + 1, day));
	}

	@Override
	public void onScreenChange(int screen) {
		page = screen;
		loadImages(page);
		tvIndex.setText(String.format("%d / %d", page + 1, day));
	}

	private void clearImages() {
		for (int i = 0; i < 21; i++) {
			if (bps[i] != null) {
				if (!bps[i].isRecycled()) {
					ivs[i].setImageBitmap(null);
					bps[i].recycle();
					bps[i] = null;
				}
			}

		}
	}

	private void loadImages(int index) {
		for (int i = 0; i < 21; i++) {
			if ((i != index) && (i != index - 1) && (i != index + 1)) {
				if (bps[i] != null) {
					if (!bps[i].isRecycled()) {
						ivs[i].setImageBitmap(null);
						bps[i].recycle();
						bps[i] = null;
					}
				}
			}
		}
		BitmapFactory.Options bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;
		if (bps[index] == null) {
			bps[index] = BitmapFactory.decodeResource(getResources(),
					bpImgs[index], bop);
			ivs[index].setImageBitmap(bps[index]);
		}

		if (index - 1 >= 0) {
			if (bps[index - 1] == null) {
				bps[index - 1] = BitmapFactory.decodeResource(getResources(),
						bpImgs[index - 1], bop);
				ivs[index - 1].setImageBitmap(bps[index - 1]);
			}
		}

		if (index + 1 < 21) {
			if (bps[index + 1] == null) {
				bps[index + 1] = BitmapFactory.decodeResource(getResources(),
						bpImgs[index + 1], bop);
				ivs[index + 1].setImageBitmap(bps[index + 1]);
			}
		}
	}

	@Override
	public void onConfigurationChanged(Configuration newConfig) {
		super.onConfigurationChanged(newConfig);
		clearImages();
		initData(page);
	}
}
