package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class SelectPictureFragment extends BaseFragment implements
		OnClickListener {

	RelativeLayout btnCamera, btnGallery;
	ImageView ivCloseDialog;

	public SelectPictureFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_select_picture_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.select_picture;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.select_picture;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		btnCamera = (RelativeLayout) innerView.findViewById(R.id.btnCamera);
		btnGallery = (RelativeLayout) innerView.findViewById(R.id.btnGallery);

		ivCloseDialog = (ImageView) innerView.findViewById(R.id.ivCloseDialog);
	}

	@Override
	public void initEvents() {
		btnCamera.setOnClickListener(this);
		btnGallery.setOnClickListener(this);
		ivCloseDialog.setOnClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_select_picture;
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

	@Override
	public void onClick(View v) {
		Intent inRet = new Intent();
		switch (v.getId()) {
		case R.id.btnCamera:
			inRet.putExtra("type", 0);
			break;
		case R.id.btnGallery:
			inRet.putExtra("type", 1);
			break;
		case R.id.ivCloseDialog:
			getActivity().finish();
			return;
		}
		getActivity().setResult(Activity.RESULT_OK, inRet);
		getActivity().finish();

	}
}
