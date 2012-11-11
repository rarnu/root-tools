package com.rarnu.zoe.loving.adapter;

import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class HistoryHolder {

	public TextView tvItem1;
	public TextView tvItem2;
	public TextView tvItem3;
	
	public ImageView imgItem1;
	public ImageView imgItem2;
	public ImageView imgItem3;

	public void setComponentSize(int width) {
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) tvItem1
				.getLayoutParams();
		rlp.width = width;
		tvItem1.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvItem2.getLayoutParams();
		rlp.width = width;
		tvItem2.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvItem3.getLayoutParams();
		rlp.width = width;
		tvItem3.setLayoutParams(rlp);
		
		rlp = (RelativeLayout.LayoutParams) imgItem1.getLayoutParams();
		rlp.width = width;
		imgItem1.setLayoutParams(rlp);
		
		rlp = (RelativeLayout.LayoutParams) imgItem2.getLayoutParams();
		rlp.width = width;
		imgItem2.setLayoutParams(rlp);
		
		rlp = (RelativeLayout.LayoutParams) imgItem3.getLayoutParams();
		rlp.width = width;
		imgItem3.setLayoutParams(rlp);

	}
}
