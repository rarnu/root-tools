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
	List<GroundInfo> list = null;
	GroundAdapter adapter = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		queryWeibo();

	}

	private void queryWeibo() {
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

		gvGround = (GridView) findViewById(R.id.gvGround);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		gvGround.setOnItemClickListener(this);
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
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		GroundInfo info = list.get(position);
		Intent inDetail = new Intent(this, GroundDetailActivity.class);
		inDetail.putExtra("index", info.id);
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

			}
		});

	}

	@Override
	public void onError(WeiboException arg0) {
		Log.e("error", arg0.getMessage());
	}

	@Override
	public void onIOException(IOException arg0) {
		Log.e("ioexception", arg0.getMessage());

	}
}
