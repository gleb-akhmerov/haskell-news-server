create table usr (
  id serial primary key,
  first_name varchar(50) not null,
  last_name varchar(50) not null,
  avatar bytea not null,
  created_at date not null,
  is_admin bool not null
);

create table author (
  id serial references usr(id) primary key,
  short_description varchar(50) not null
);

create table category (
  id serial primary key,
  parent_id integer references category(id),
  name varchar(50) unique not null
);

create table photo (
  id serial primary key,
  content bytea not null
);

create table draft (
  id serial primary key,
  short_name varchar(50) not null,
  created_at date not null,
  author_id serial references author(id) not null,
  category_id serial references category(id) not null,
  text_content text not null,
  main_photo_id serial references photo(id) not null
);

create table post (
  id serial primary key,
  draft_id serial references draft(id) not null,
  short_name varchar(50) not null,
  published_at date not null,
  author_id serial references author(id) not null,
  category_id serial references category(id) not null,
  text_content text not null,
  main_photo_id serial references photo(id) not null
);

create table tag (
  id serial primary key,
  name varchar(50) not null
);

create table post_tag (
  tag_id serial references tag(id),
  post_id serial references post(id),
  primary key (tag_id, post_id)
);

create table draft_tag (
  tag_id serial references tag(id),
  draft_id serial references draft(id),
  primary key (tag_id, draft_id)
);

create table post_additional_photo (
  photo_id serial references photo(id),
  post_id serial references post(id),
  primary key (photo_id, post_id)
);

create table draft_additional_photo (
  photo_id serial references photo(id),
  draft_id serial references draft(id),
  primary key (photo_id, draft_id)
);

create table commentary (
  id serial primary key,
  user_id serial references usr(id) not null,
  post_id serial references post(id) not null,
  content text not null
);
