package com.rarnu.zoe.love2;

import java.io.File;

import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.love2.api.LovingYouApi;
import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.GroundInfo;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.DownloadUtils;
import com.rarnu.zoe.love2.utils.ShareUtils;

public class GroundDetailActivity extends BaseActivity implements
		OnClickListener {

	ImageView imgPhoto;
	TextView tvDesc;
	ImageView imgShare;

	RelativeLayout layLoading;
	GroundInfo info = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		info = (GroundInfo) getIntent().getSerializableExtra("data");
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
		imgShare = (ImageView) findViewById(R.id.imgShare);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		imgShare.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			LovingYouApi.saveLog(this, "GroundDetailActivity", "Back");
			finish();
			break;
		case R.id.imgShare:
			// share
			LovingYouApi.saveLog(this, "GroundDetailActivity", "Share");
			ShareUtils.shareTo(
					this,
					getString(R.string.share),
					getString(R.string.share_title),
					info.txt,
					Uri.fromFile(new File(DownloadUtils.SAVE_PATH
							+ String.format("b_%s.jpg", info.id))), null);
			break;
		}
	}

	@Override
	protected void onDestroy() {
		imgPhoto.setImageBitmap(null);
		super.onDestroy();
	}

}
