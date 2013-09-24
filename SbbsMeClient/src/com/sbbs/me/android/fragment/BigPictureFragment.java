package com.sbbs.me.android.fragment;

import java.io.File;

import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.utils.MiscUtils;

public class BigPictureFragment extends BaseFragment {

	ImageView ivImage;
	ProgressBar pbImage;

	public BigPictureFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_big_picture_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.view_image;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.view_image;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		ivImage = (ImageView) innerView.findViewById(R.id.ivImage);
		pbImage = (ProgressBar) innerView.findViewById(R.id.pbImage);
	}

	@Override
	public void initEvents() {

	}

	private Handler hProgress = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			switch (msg.what) {
			case DownloadUtils.WHAT_DOWNLOAD_START:
				pbImage.setMax(msg.arg2);
				pbImage.setProgress(msg.arg1);
				pbImage.setVisibility(View.VISIBLE);
				break;
			case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
				pbImage.setMax(msg.arg2);
				pbImage.setProgress(msg.arg1);
				break;
			case DownloadUtils.WHAT_DOWNLOAD_FINISH:
				pbImage.setVisibility(View.GONE);
				break;
			}
			super.handleMessage(msg);
		};
	};

	@Override
	public void initLogic() {
		String image = getArguments().getString("image");
		String localImage = PathDefine.ROOT_PATH
				+ MiscUtils.extractFileNameFromURL(image);
		if (!new File(localImage).exists()) {

			DownloadUtils.downloadFileT(getActivity(), ivImage, image,
					PathDefine.ROOT_PATH,
					MiscUtils.extractFileNameFromURL(image), hProgress);
		} else {
			ivImage.setImageBitmap(BitmapFactory.decodeFile(localImage));
		}

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_big_picture;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
