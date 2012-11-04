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

import com.rarnu.zoe.loving.GalleryActivity;
import com.rarnu.zoe.loving.Global;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.adapter.ImageAdapter;
import com.rarnu.zoe.loving.base.BasePage;

public class PageImage extends BasePage implements OnItemClickListener {

	GridView gvImages;
	List<Integer> list = null;
	ImageAdapter adapter = null;
	
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
		list = new ArrayList<Integer>();
		int day = Global.database.getDay();
		for (int i=0; i<day; i++) {
			list.add(0);
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
	public void onItemClick(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
		Intent inGallery = new Intent(getContext(), GalleryActivity.class);
		getContext().startActivity(inGallery);
		
	}

}
