package com.rarnu.zoe.loving.page;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.util.AttributeSet;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;

import com.rarnu.zoe.loving.AboutActivity;
import com.rarnu.zoe.loving.GalleryActivity;
import com.rarnu.zoe.loving.Global;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.adapter.ImageAdapter;
import com.rarnu.zoe.loving.base.BasePage;
import com.rarnu.zoe.loving.common.ImageInfo;

public class PageImage extends BasePage implements OnItemClickListener {

	GridView gvImages;
	List<ImageInfo> list = null;
	ImageAdapter adapter = null;

	int[] spImgs = new int[] { R.drawable.sp1, R.drawable.sp2, R.drawable.sp3,
			R.drawable.sp4, R.drawable.sp5, R.drawable.sp6, R.drawable.sp7,
			R.drawable.sp8, R.drawable.sp9, R.drawable.sp10, R.drawable.sp11,
			R.drawable.sp12, R.drawable.sp13, R.drawable.sp14, R.drawable.sp15,
			R.drawable.sp16, R.drawable.sp17, R.drawable.sp18, R.drawable.sp19,
			R.drawable.sp20, R.drawable.sp21 };

	public PageImage(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public PageImage(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PageImage(Context context) {
		super(context);
	}

	@Override
	protected void requireRootLayoutId() {
		this.rootLayout = R.layout.page_image;
	}

	@Override
	protected void init() {
		gvImages = (GridView) findViewById(R.id.gvImages);
		gvImages.setOnItemClickListener(this);
	}

	@Override
	public void load(String... param) {
		list = new ArrayList<ImageInfo>();
		int day = Global.database.getDay();
		day = 15;
		for (int i = 0; i < 21; i++) {
			ImageInfo info = new ImageInfo();
			// TODO: image index
			info.image = spImgs[i];
			if (i == 20) {
				info.text = getContext().getString(R.string.about);
			} else {
				info.text = String.valueOf(21 - i);
			}
			info.showImage = ((i + 1) <= day);
			list.add(info);
		}

		adapter = new ImageAdapter(getContext(), list);
		gvImages.setAdapter(adapter);

	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		Intent inDo = null;
		if (position == 20 && Global.database.getDay() < 21) {
			inDo = new Intent(getContext(), AboutActivity.class);
		} else if (position < Global.database.getDay()) {
			inDo = new Intent(getContext(), GalleryActivity.class);
			inDo.putExtra("index", position);
		}
		if (inDo != null) {
			getContext().startActivity(inDo);
		}
	}
}
