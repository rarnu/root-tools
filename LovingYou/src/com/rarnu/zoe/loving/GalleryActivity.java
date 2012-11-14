package com.rarnu.zoe.loving;

import android.app.Activity;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

import com.rarnu.zoe.loving.comp.ScrollLayout;
import com.rarnu.zoe.loving.comp.ScrollLayout.OnScreenChangeListener;
import com.rarnu.zoe.loving.page.PageTodo;

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

	PageTodo[] ivs = new PageTodo[21];

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
			ivs[i] = new PageTodo(this);
			ivs[i].setLayoutParams(new RelativeLayout.LayoutParams(
					LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT));
			ivs[i].load(String.valueOf(i));
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

	private void loadImages(int index) {
		for (int i = 0; i < 21; i++) {
			if ((i != index) && (i != index - 1) && (i != index + 1)) {
				ivs[i].loadImage(0);
			}
		}
		BitmapFactory.Options bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;

		ivs[index].loadImage(bpImgs[index]);

		if (index - 1 >= 0) {
			ivs[index - 1].loadImage(bpImgs[index - 1]);
		}

		if (index + 1 < 21) {
			ivs[index + 1].loadImage(bpImgs[index + 1]);
		}
	}
}
