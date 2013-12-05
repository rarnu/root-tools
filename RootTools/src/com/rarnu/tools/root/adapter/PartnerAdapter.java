package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;

public class PartnerAdapter extends BaseAdapter<Integer> {

	public PartnerAdapter(Context context, List<Integer> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = null;
        switch (position) {
            case 0:
                v = inflater.inflate(R.layout.partner_item_eoe, parent, false);
                break;
            case 1:
                v = inflater.inflate(R.layout.partner_item_ucloud, parent, false);
                break;
        }
		return v;
	}

	@Override
	public String getValueText(Integer item) {
		return "";
	}

}
