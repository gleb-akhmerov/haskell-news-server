-- Move avatars from user column to photo. Now users reference avatars by photo id.

alter table photo
add usr_id integer references usr(id);

alter table usr
add avatar_id serial references photo(id);

insert into photo (usr_id, content)
select id, avatar from usr;

update usr
set avatar_id = p.id
from (select * from photo) as p
where usr.id = p.usr_id;

alter table usr
drop column avatar;

alter table photo
drop column usr_id;
