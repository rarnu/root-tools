package com.rarnu.vim.emotion.fragment;

import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.widget.ImageView;
import android.widget.Toast;

import com.rarnu.vim.emotion.Global;
import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.common.base.BaseFragment;
import com.rarnu.vim.emotion.utils.MiscUtils;

public class CenterFragment extends BaseFragment implements
		OnLongClickListener, OnClickListener {

	ImageView imgEmotion;
	ImageView icoLeft, icoRight, icoTop, icoBottom;

	@Override
	public int getFragmentLayout() {
		return R.layout.layout_center;
	}

	@Override
	public void initComponents() {
		imgEmotion = (ImageView) innerView.findViewById(R.id.imgEmotion);
		icoLeft = (ImageView) innerView.findViewById(R.id.icoLeft);
		icoRight = (ImageView) innerView.findViewById(R.id.icoRight);
		icoTop = (ImageView) innerView.findViewById(R.id.icoTop);
		icoBottom = (ImageView) innerView.findViewById(R.id.icoBottom);

		imgEmotion.setOnLongClickListener(this);
		icoLeft.setOnClickListener(this);
		icoRight.setOnClickListener(this);
		icoTop.setOnClickListener(this);
		icoBottom.setOnClickListener(this);
	}

	@Override
	public void init() {
		setCurrentFace(Global.face.value);
	}

	public void setCurrentFace(String faceName) {
		imgEmotion.setBackground(MiscUtils.getBitmapByAssets(getActivity(),
				"face/" + faceName));
	}

	@Override
	public boolean onLongClick(View v) {
		setIconVisible(icoLeft.getVisibility() != View.VISIBLE);
		return true;
	}

	private void setIconVisible(boolean visible) {
		if (visible) {
			icoLeft.setVisibility(View.VISIBLE);
			icoRight.setVisibility(View.VISIBLE);
			icoTop.setVisibility(View.VISIBLE);
			icoBottom.setVisibility(View.VISIBLE);
		} else {
			icoLeft.setVisibility(View.GONE);
			icoRight.setVisibility(View.GONE);
			icoTop.setVisibility(View.GONE);
			icoBottom.setVisibility(View.GONE);
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.icoLeft:
			Toast.makeText(getActivity(), "LEFT", Toast.LENGTH_SHORT).show();
			break;
		case R.id.icoRight:
			Toast.makeText(getActivity(), "RIGHT", Toast.LENGTH_SHORT).show();
			break;
		case R.id.icoTop:
			Toast.makeText(getActivity(), "TOP", Toast.LENGTH_SHORT).show();
			break;
		case R.id.icoBottom:
			Toast.makeText(getActivity(), "BOTTOM", Toast.LENGTH_SHORT).show();
			break;
		}
		setIconVisible(false);
	}

	@Override
	public void doShrink() {
		
	}

	@Override
	public void doExpand() {
		
	}

}
