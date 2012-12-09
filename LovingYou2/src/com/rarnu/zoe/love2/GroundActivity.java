package com.rarnu.zoe.love2;

import java.io.IOException;
import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.zoe.love2.adapter.GroundAdapter;
import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.GroundInfo;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.WeiboUtils;
import com.weibo.sdk.android.WeiboException;
import com.weibo.sdk.android.net.RequestListener;

public class GroundActivity extends BaseActivity implements OnClickListener,
		OnItemClickListener, RequestListener {

	GridView gvGround;
	RelativeLayout layLoading;
	ImageView imgAddFeedback;

	List<GroundInfo> list = null;
	GroundAdapter adapter = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		queryWeibo();

	}

	private void queryWeibo() {
		layLoading.setVisibility(View.VISIBLE);
		WeiboUtils.getWeiboList(this);
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_ground);

	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.ground);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);
		title.getBarItem(Title.BARITEM_RIGHT).setIcon(R.drawable.task_top);

		gvGround = (GridView) findViewById(R.id.gvGround);
		layLoading = (RelativeLayout) findViewById(R.id.layLoading);

		imgAddFeedback = (ImageView) findViewById(R.id.imgAddFeedback);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		title.getBarItem(Title.BARITEM_RIGHT).setOnButtonClick(this);
		gvGround.setOnItemClickListener(this);
		imgAddFeedback.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		case Title.ITEM_RIGHT:
			Intent inRecord = new Intent(this, RecordActivity.class);
			startActivity(inRecord);
			break;
		case R.id.imgAddFeedback:
			Intent inFeedback = new Intent(this, FeedbackActivity.class);
			startActivity(inFeedback);
			break;
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		GroundInfo info = list.get(position);
		Intent inDetail = new Intent(this, GroundDetailActivity.class);
		inDetail.putExtra("data", info);
		startActivity(inDetail);
	}

	@Override
	public void onComplete(String arg0) {
		list = WeiboUtils.extractGroundList(arg0);
		adapter = new GroundAdapter(this, list);
		runOnUiThread(new Runnable() {

			@Override
			public void run() {
				gvGround.setAdapter(adapter);
				layLoading.setVisibility(View.GONE);

			}
		});

	}

	@Override
	public void onError(WeiboException arg0) {
		Log.e("error", arg0.getMessage());
		runOnUiThread(new Runnable() {

			@Override
			public void run() {
				layLoading.setVisibility(View.GONE);

			}
		});
	}

	@Override
	public void onIOException(IOException arg0) {
		Log.e("ioexception", arg0.getMessage());
		runOnUiThread(new Runnable() {

			@Override
			public void run() {
				layLoading.setVisibility(View.GONE);

			}
		});
	}
}
