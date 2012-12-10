package com.rarnu.zoe.love2;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.GroundInfo;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.DownloadUtils;

public class GroundDetailActivity extends BaseActivity implements
		OnClickListener {

	ImageView imgPhoto;
	TextView tvDesc;

	RelativeLayout layLoading;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		GroundInfo info = (GroundInfo) getIntent().getSerializableExtra("data");
		if (info == null) {
			finish();
			return;
		}
		// load
		tvDesc.setText(info.txt);
		DownloadUtils.downloadFileT(this, info, imgPhoto, 1, layLoading);
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_ground_detail);

	}

	@Override
	protected void initComponents() {
		super.initComponents();
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.task_detail);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);

		imgPhoto = (ImageView) findViewById(R.id.imgPhoto);
		tvDesc = (TextView) findViewById(R.id.tvDesc);
		layLoading = (RelativeLayout) findViewById(R.id.layLoading);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		}
	}

	@Override
	protected void onDestroy() {
		imgPhoto.setImageBitmap(null);
		super.onDestroy();
	}

}
