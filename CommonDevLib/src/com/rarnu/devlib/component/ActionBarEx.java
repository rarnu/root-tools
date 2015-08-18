package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.R;
import com.rarnu.utils.UIUtils;

/**
 * Created by rarnu on 2/6/15.
 */
public class ActionBarEx extends RelativeLayout implements View.OnClickListener, View.OnLongClickListener {

    @Override
    public boolean onLongClick(View v) {
        String tag = (String) v.getTag();
        if (tag != null && !tag.equals("")) {
            // toast tag
        }
        return true;
    }

    public interface ActionBarExListener {
        void onActionButtonClicked(ActionBarEx bar, int buttonId);

        void onActionTitleClicked(ActionBarEx bar);
    }

    private TextView tvTitle;
    private ImageButton btnMostLeft;
    private ImageButton btnLeft;
    private ImageButton btnMostRight;
    private ImageButton btnRight;
    private ActionBarExListener listener;

    public static final int ID_BTN_MOST_LEFT = 10001;
    public static final int ID_BTN_LEFT = 10002;
    public static final int ID_BTN_RIGHT = 10003;
    public static final int ID_BTN_MOST_RIGHT = 10004;

    public ActionBarEx(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public ActionBarEx(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public ActionBarEx(Context context) {
        super(context);
        init();
    }

    private void init() {
        tvTitle = new TextView(getContext());
        tvTitle.setLayoutParams(new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
        tvTitle.setGravity(Gravity.CENTER);
        tvTitle.setTextSize(18);
        tvTitle.setTextColor(Color.WHITE);
        tvTitle.setPadding(UIUtils.dipToPx(8), 0, UIUtils.dipToPx(8), 0);
        addView(tvTitle);

        btnMostLeft = new ImageButton(getContext());
        btnMostLeft.setId(ID_BTN_MOST_LEFT);
        RelativeLayout.LayoutParams rllpMostLeft = new RelativeLayout.LayoutParams(UIUtils.dipToPx(48), LayoutParams.MATCH_PARENT);
        rllpMostLeft.addRule(RelativeLayout.ALIGN_PARENT_LEFT, RelativeLayout.TRUE);
        btnMostLeft.setLayoutParams(rllpMostLeft);
        btnMostLeft.setBackgroundResource(R.drawable.btn_cab_done);
        btnMostLeft.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
        btnMostLeft.setVisibility(View.VISIBLE);
        addView(btnMostLeft);

        btnLeft = new ImageButton(getContext());
        btnLeft.setId(ID_BTN_LEFT);
        RelativeLayout.LayoutParams rllpLeft = new RelativeLayout.LayoutParams(UIUtils.dipToPx(48), LayoutParams.MATCH_PARENT);
        rllpMostLeft.addRule(RelativeLayout.RIGHT_OF, ID_BTN_MOST_LEFT);
        btnLeft.setLayoutParams(rllpLeft);
        btnLeft.setBackgroundResource(R.drawable.btn_cab_done);
        btnLeft.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
        btnLeft.setVisibility(View.GONE);
        addView(btnLeft);

        btnMostRight = new ImageButton(getContext());
        btnMostRight.setId(ID_BTN_MOST_RIGHT);
        RelativeLayout.LayoutParams rllpMostRight = new RelativeLayout.LayoutParams(UIUtils.dipToPx(48), LayoutParams.MATCH_PARENT);
        rllpMostRight.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, RelativeLayout.TRUE);
        btnMostRight.setLayoutParams(rllpMostRight);
        btnMostRight.setBackgroundResource(R.drawable.btn_cab_done);
        btnMostRight.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
        btnMostRight.setVisibility(View.GONE);
        addView(btnMostRight);

        btnRight = new ImageButton(getContext());
        btnRight.setId(ID_BTN_RIGHT);
        RelativeLayout.LayoutParams rllpRight = new RelativeLayout.LayoutParams(UIUtils.dipToPx(48), LayoutParams.MATCH_PARENT);
        rllpRight.addRule(RelativeLayout.LEFT_OF, ID_BTN_MOST_RIGHT);
        btnRight.setLayoutParams(rllpRight);
        btnRight.setBackgroundResource(R.drawable.btn_cab_done);
        btnRight.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
        btnRight.setVisibility(View.GONE);
        addView(btnRight);

        btnMostLeft.setOnClickListener(this);
        btnLeft.setOnClickListener(this);
        btnRight.setOnClickListener(this);
        btnMostRight.setOnClickListener(this);
        tvTitle.setOnClickListener(this);

        btnMostLeft.setOnLongClickListener(this);
        btnRight.setOnLongClickListener(this);
        btnLeft.setOnLongClickListener(this);
        btnMostRight.setOnLongClickListener(this);

    }

    public void setActionBarListener(ActionBarExListener l) {
        listener = l;
    }

    public void setTitle(String text) {
        tvTitle.setText(text);
    }

    public void setTitle(int res) {
        tvTitle.setText(res);
    }

    public void setTitleColor(int res) {
        tvTitle.setTextColor(getResources().getColor(res));
    }

    @Override
    public void onClick(View v) {
        if (listener != null) {
            if (v instanceof TextView) {
                listener.onActionTitleClicked(this);
            } else if (v instanceof ImageButton) {
                listener.onActionButtonClicked(this, v.getId());
            }
        }
    }

    public void addButton(int position, int title, int icon) {
        addButton(position, getResources().getString(title), getResources().getDrawable(icon));
    }

    public void addButton(int position, String title, int icon) {
        addButton(position, title, getResources().getDrawable(icon));
    }

    public void addButton(int position, int title, Drawable icon) {
        addButton(position, getResources().getString(title), icon);
    }

    public void addButton(int position, String title, Drawable icon) {
        switch (position) {
            case ID_BTN_MOST_LEFT:
                btnMostLeft.setTag(title);
                btnMostLeft.setImageDrawable(icon);
                btnMostLeft.setVisibility(View.VISIBLE);
                break;
            case ID_BTN_LEFT:
                btnLeft.setTag(title);
                btnLeft.setImageDrawable(icon);
                btnLeft.setVisibility(View.VISIBLE);
                break;
            case ID_BTN_MOST_RIGHT:
                btnMostRight.setTag(title);
                btnMostRight.setImageDrawable(icon);
                btnMostRight.setVisibility(View.VISIBLE);
                break;
            case ID_BTN_RIGHT:
                btnRight.setTag(title);
                btnRight.setImageDrawable(icon);
                btnRight.setVisibility(View.VISIBLE);
                break;
        }
    }

    public void removeButton(int position) {
        switch (position) {
            case ID_BTN_MOST_LEFT:
                btnMostLeft.setVisibility(View.GONE);
                break;
            case ID_BTN_LEFT:
                btnLeft.setVisibility(View.GONE);
                break;
            case ID_BTN_MOST_RIGHT:
                btnMostRight.setVisibility(View.GONE);
                break;
            case ID_BTN_RIGHT:
                btnRight.setVisibility(View.GONE);
                break;
        }
    }

    public void setTitleGravity(int g) {
        tvTitle.setGravity(g);
    }

    public void setButtonEnabled(int position, boolean e) {
        switch (position) {
            case ID_BTN_MOST_LEFT:
                btnMostLeft.setEnabled(e);
                break;
            case ID_BTN_LEFT:
                btnLeft.setEnabled(e);
                break;
            case ID_BTN_MOST_RIGHT:
                btnMostRight.setEnabled(e);
                break;
            case ID_BTN_RIGHT:
                btnRight.setEnabled(e);
                break;
        }
    }
}
