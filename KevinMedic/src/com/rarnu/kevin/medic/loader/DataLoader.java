package com.rarnu.kevin.medic.loader;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.kevin.medic.R;

public class DataLoader extends BaseLoader<String> {

	private int type;

	public DataLoader(Context context, int type) {
		super(context);
		this.type = type;
	}

	@Override
	public List<String> loadInBackground() {
		List<String> lst = new ArrayList<String>();
		String base = "";
		switch (type) {
		case 2:
			base = getContext().getString(R.string.leftmenu_1);
			break;
		case 3:
			base = getContext().getString(R.string.leftmenu_2);
			break;
		case 4:
			base = getContext().getString(R.string.leftmenu_3);
			break;
		case 5:
			base = getContext().getString(R.string.leftmenu_4);
			break;
		}
		for (int i = 0; i < 10; i++) {
			lst.add(base + " " + String.valueOf(i + 1));
		}
		return lst;
	}

}
