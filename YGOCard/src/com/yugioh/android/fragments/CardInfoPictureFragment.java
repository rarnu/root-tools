package com.yugioh.android.fragments;

import java.io.File;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.utils.DownloadUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.define.NetworkDefine;
import com.yugioh.android.define.PathDefine;

public class CardInfoPictureFragment extends BaseFragment implements
		OnClickListener {

	CardInfo info;

	ImageView ivImage;
	TextView tvNoPic;
	ProgressBar pbDownload;

	public CardInfoPictureFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	protected void initComponents() {
		ivImage = (ImageView) innerView.findViewById(R.id.ivImage);
		tvNoPic = (TextView) innerView.findViewById(R.id.tvNoPic);
		pbDownload = (ProgressBar) innerView.findViewById(R.id.pbDownload);
	}

	@Override
	protected void initEvents() {
		tvNoPic.setOnClickListener(this);
	}

	@Override
	protected void initLogic() {
		info = (CardInfo) getActivity().getIntent().getSerializableExtra(
				"cardinfo");
		String picName = PathDefine.PICTURE_PATH
				+ String.format("%d.jpg", info.getCardID() - 1);
		File fPic = new File(picName);
		if (fPic.exists()) {
			Bitmap cardImg = BitmapFactory.decodeFile(picName);
			ivImage.setImageBitmap(cardImg);
			ivImage.setVisibility(View.VISIBLE);
			tvNoPic.setVisibility(View.GONE);
		} else {
			ivImage.setVisibility(View.GONE);
			tvNoPic.setVisibility(View.VISIBLE);
		}
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_cardinfo_pic;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public String getCustomTitle() {
		String title = null;
		if (info != null) {
			title = info.getSCCardName();
		}
		return title;
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvNoPic:
			doDownloadT();
			break;
		}
	}

	private Handler hDownloadProgress = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			switch (msg.what) {
			case DownloadUtils.WHAT_DOWNLOAD_START:
			case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
				pbDownload.setMax(msg.arg2);
				pbDownload.setProgress(msg.arg1);
				break;
			case DownloadUtils.WHAT_DOWNLOAD_FINISH:
				pbDownload.setVisibility(View.GONE);
				ivImage.setVisibility(View.VISIBLE);
				break;
			}
			super.handleMessage(msg);
		};
	};

	private void doDownloadT() {
		pbDownload.setProgress(0);
		pbDownload.setVisibility(View.VISIBLE);
		tvNoPic.setVisibility(View.GONE);
		String url = String.format(NetworkDefine.URL_CARD_IMAGE_FMT,
				info.getCardID() - 1);
		String localDir = PathDefine.PICTURE_PATH;
		String localFile = String.format("%d.jpg", info.getCardID() - 1);
		DownloadUtils.downloadFileT(getActivity(), ivImage, url, localDir,
				localFile, hDownloadProgress);
	}

}
