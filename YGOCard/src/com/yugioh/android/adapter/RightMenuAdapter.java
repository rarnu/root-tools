package com.yugioh.android.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.yugioh.android.R;
import com.yugioh.android.classes.RightMenuItem;

import java.util.List;

public class RightMenuAdapter extends BaseAdapter<RightMenuItem> {

    public RightMenuAdapter(Context context, List<RightMenuItem> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_menu_right, parent, false);
        }
        RightMenuHolder holder = (RightMenuHolder) v.getTag();
        if (holder == null) {
            holder = new RightMenuHolder();
            holder.tvMenuName = (TextView) v.findViewById(R.id.tvMenuName);
            holder.ivImg = (ImageView) v.findViewById(R.id.ivImg);
            v.setTag(holder);
        }

        RightMenuItem item = list.get(position);
        if (item != null) {
            holder.tvMenuName.setText(item.name);
            switch (item.type) {
                case 0:
                    if (item.value == 0) {
                        holder.ivImg.setImageDrawable(null);

                    } else {
                        holder.ivImg.setImageResource(android.R.drawable.ic_menu_upload);
                    }
                    break;
            }
        }

        return v;
    }

    @Override
    public String getValueText(RightMenuItem item) {
        return "";
    }

}
