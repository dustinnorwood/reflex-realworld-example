--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.12
-- Dumped by pg_dump version 9.6.12

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: article_tags; Type: TABLE; Schema: public; Owner: conduit
--

CREATE TABLE public.article_tags (
    article__id integer,
    tag__name text
);


ALTER TABLE public.article_tags OWNER TO conduit;

--
-- Name: articles; Type: TABLE; Schema: public; Owner: conduit
--

CREATE TABLE public.articles (
    id integer NOT NULL,
    body text NOT NULL,
    slug text NOT NULL,
    title text NOT NULL,
    description text NOT NULL,
    author__id integer,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE public.articles OWNER TO conduit;

--
-- Name: articles_id_seq; Type: SEQUENCE; Schema: public; Owner: conduit
--

CREATE SEQUENCE public.articles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.articles_id_seq OWNER TO conduit;

--
-- Name: articles_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: conduit
--

ALTER SEQUENCE public.articles_id_seq OWNED BY public.articles.id;


--
-- Name: comments; Type: TABLE; Schema: public; Owner: conduit
--

CREATE TABLE public.comments (
    id integer NOT NULL,
    body text NOT NULL,
    author__id integer,
    article__id integer,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE public.comments OWNER TO conduit;

--
-- Name: comments_id_seq; Type: SEQUENCE; Schema: public; Owner: conduit
--

CREATE SEQUENCE public.comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comments_id_seq OWNER TO conduit;

--
-- Name: comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: conduit
--

ALTER SEQUENCE public.comments_id_seq OWNED BY public.comments.id;


--
-- Name: favorites; Type: TABLE; Schema: public; Owner: conduit
--

CREATE TABLE public.favorites (
    user__id integer,
    article__id integer
);


ALTER TABLE public.favorites OWNER TO conduit;

--
-- Name: follows; Type: TABLE; Schema: public; Owner: conduit
--

CREATE TABLE public.follows (
    follower__id integer,
    followee__id integer
);


ALTER TABLE public.follows OWNER TO conduit;

--
-- Name: tags; Type: TABLE; Schema: public; Owner: conduit
--

CREATE TABLE public.tags (
    name text NOT NULL
);


ALTER TABLE public.tags OWNER TO conduit;

--
-- Name: users; Type: TABLE; Schema: public; Owner: conduit
--

CREATE TABLE public.users (
    id integer NOT NULL,
    password text NOT NULL,
    email text NOT NULL,
    username text NOT NULL,
    bio text NOT NULL,
    image text
);


ALTER TABLE public.users OWNER TO conduit;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: conduit
--

CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO conduit;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: conduit
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: articles id; Type: DEFAULT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.articles ALTER COLUMN id SET DEFAULT nextval('public.articles_id_seq'::regclass);


--
-- Name: comments id; Type: DEFAULT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.comments ALTER COLUMN id SET DEFAULT nextval('public.comments_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Data for Name: article_tags; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.article_tags (article__id, tag__name) FROM stdin;
3	parties
4	being cool
\.


--
-- Data for Name: articles; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.articles (id, body, slug, title, description, author__id, created_at, updated_at) FROM stdin;
3	# Things that you will need\n\n- Cake\n- Party Cannon	how-to-throw-an-awesome-party	How to Throw an Awesome Party!	Party organising techniques	7	2019-05-07 23:48:45.982494+00	2019-05-07 23:48:45.982494+00
4	TODO	how-to-become-at-least-20-cooler	How to become (at least) 20% Cooler	Being Cool	5	2019-05-07 23:51:44.856755+00	2019-05-07 23:51:44.856755+00
\.


--
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: public; Owner: conduit
--

SELECT pg_catalog.setval('public.articles_id_seq', 4, true);


--
-- Data for Name: comments; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.comments (id, body, author__id, article__id, created_at, updated_at) FROM stdin;
4	This article is still not finished. Please remember to finish it off soon. :)	6	4	2019-05-12 15:25:21.996366+00	2019-05-12 15:25:21.996366+00
5	Oh yeah... Will do, thanks! 	5	4	2019-05-12 15:26:17.483094+00	2019-05-12 15:26:17.483094+00
\.


--
-- Name: comments_id_seq; Type: SEQUENCE SET; Schema: public; Owner: conduit
--

SELECT pg_catalog.setval('public.comments_id_seq', 5, true);


--
-- Data for Name: favorites; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.favorites (user__id, article__id) FROM stdin;
\.


--
-- Data for Name: follows; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.follows (follower__id, followee__id) FROM stdin;
5	6
6	5
5	7
\.


--
-- Data for Name: tags; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.tags (name) FROM stdin;
parties
being cool
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.users (id, password, email, username, bio, image) FROM stdin;
5	14|8|1|9eJWoRDVyc5sKteli5y+LE0aFO4g1LhgD1ol3Yy2Tvg=|Uq8llEuBLZtdQ5pvPidP4VkeGpd1CqdeEJ+NFOcVRfeFYaDbftDxhY5sX05zdHc2nzoalzsjKgzdeFmcVcTSzw==	dashy@mlp	Dashy	Ponyville weather control expert & Trainee Wonderbolt!	/static/avatars/dashy.png
7	14|8|1|uyRA6mhsCBcVPywd8F1Jsqe8BE4bVgIthD5NwLhXCgw=|7+5+h+9OKFb68D1zvmnUkmQ0q7liuLwfKz1TCH07DBe7BH7Za9wQ65iSs8Peedfi0i6PgWrXAtqrCsnON83+zQ==	pinkiepie@mlp	PinkiePie	Loves to throw parties!	/static/avatars/pinkie.png
6	14|8|1|9eJWoRDVyc5sKteli5y+LE0aFO4g1LhgD1ol3Yy2Tvg=|Uq8llEuBLZtdQ5pvPidP4VkeGpd1CqdeEJ+NFOcVRfeFYaDbftDxhY5sX05zdHc2nzoalzsjKgzdeFmcVcTSzw==	fluttershy@mlp	Fluttershy		/static/avatars/fluttershy.png
\.


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: conduit
--

SELECT pg_catalog.setval('public.users_id_seq', 7, true);


--
-- Name: article_tags article_tags_article__id_tag__name_key; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.article_tags
    ADD CONSTRAINT article_tags_article__id_tag__name_key UNIQUE (article__id, tag__name);


--
-- Name: articles articles_pkey; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


--
-- Name: articles articles_slug_key; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_slug_key UNIQUE (slug);


--
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- Name: tags tags_pkey; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_pkey PRIMARY KEY (name);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: article_tags article_tags_article__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.article_tags
    ADD CONSTRAINT article_tags_article__id_fkey FOREIGN KEY (article__id) REFERENCES public.articles(id) ON DELETE CASCADE;


--
-- Name: article_tags article_tags_tag__name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.article_tags
    ADD CONSTRAINT article_tags_tag__name_fkey FOREIGN KEY (tag__name) REFERENCES public.tags(name) ON DELETE CASCADE;


--
-- Name: articles articles_author__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_author__id_fkey FOREIGN KEY (author__id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: comments comments_article__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_article__id_fkey FOREIGN KEY (article__id) REFERENCES public.articles(id) ON DELETE CASCADE;


--
-- Name: comments comments_author__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_author__id_fkey FOREIGN KEY (author__id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: favorites favorites_article__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.favorites
    ADD CONSTRAINT favorites_article__id_fkey FOREIGN KEY (article__id) REFERENCES public.articles(id) ON DELETE CASCADE;


--
-- Name: favorites favorites_user__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.favorites
    ADD CONSTRAINT favorites_user__id_fkey FOREIGN KEY (user__id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: follows follows_followee__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.follows
    ADD CONSTRAINT follows_followee__id_fkey FOREIGN KEY (followee__id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: follows follows_follower__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: conduit
--

ALTER TABLE ONLY public.follows
    ADD CONSTRAINT follows_follower__id_fkey FOREIGN KEY (follower__id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--


insert into public.tags (name) values ('Food Adventures'), ('Art Appreciators'), ('Thrill Seekers');
CREATE TABLE public.package_tags (
    package__id integer,
    tag__name text
);
ALTER TABLE public.package_tags OWNER TO conduit;
CREATE TABLE public.packages (
    id integer NOT NULL,
    image text NOT NULL,
    body text NOT NULL,
    slug text NOT NULL,
    title text NOT NULL,
    description text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
ALTER TABLE public.packages OWNER TO conduit;
CREATE SEQUENCE public.packages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE public.packages_id_seq OWNER TO conduit;
ALTER SEQUENCE public.packages_id_seq OWNED BY public.packages.id;
CREATE TABLE public.wishlists (
    user__id integer,
    package__id integer
);
ALTER TABLE public.wishlists OWNER TO conduit;
ALTER TABLE ONLY public.packages ALTER COLUMN id SET DEFAULT nextval('public.packages_id_seq'::regclass);
insert into public.package_tags (package__id, tag__name) values (3, 'Food Adventures'), (4, 'Art Appreciators'), (5, 'Thrill Seekers');
insert into public.tags (name) values ('popular'), ('categories'), ('top-rated'),('we-think-you-will-like');
insert into public.packages (id, image, body, slug, title, description, created_at, updated_at) values
(3,'/static/packages/burger_pursuit.png','# Things that you will need\n\n- Cake\n- Party Cannon','burger-pursuit','Burger Pursuit','Ketchup to these patties in this all-out arms race to the best burger bars in Dallas','2021-03-27 23:48:45.982494+00','2021-03-27 23:48:45.982494+00'),
(4,'/static/packages/dinos_and_drinks.png','TODO','dinos-and-drinks','Dinos and Drinks','Bruh... muhfuckin Dinos!','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00'),
(5,'/static/packages/animals_of_dallas.png','TODO','animals-of-dallas','Animals of Dallas','Do you live under a rock? Come hang with some of your own kind that literally do.','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00'),
(6,'/static/packages/beef_hunt.png','TODO','beef-hunt','Beef Hunt','Your voices have been herd: all aboard the stock train to Tenderville! (all well-done requests will be met with a condescending guffaw and a lifetime ban from these bovine-centric establishments)','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00'),
(7,'/static/packages/tacos_and_talks.png','TODO','tacos-and-talks','Tacos and Talks','No, this isn''t your drunk cousin cooking velveeta and rotel while blathering about their workplace drama. Come listen to certified TEDx speakers blather about _their_ workplace drama while indulging in one of the few things Texas does right.','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00'),
(8,'/static/packages/zoo_stuff.png','TODO','zoo-stuff','Zoo Stuff','We couldn''t come up with a better name for this one?','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00'),
(9,'/static/packages/girls_girls_girls.png','TODO','girls-girls-girls','Girls Girls Girls','There''s nothing religious about this Easter Egg! Come party with the best bunnies Dallas has to offer, if that''s what you''re into.','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00'),
(10,'/static/packages/water_world.png','TODO','water-world','Water World','Basically our Girls Girls Girls package for the whole family.','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00'),
(11,'/static/packages/tipsy_times.png','TODO','tipsy-times','Tipsy Time','The best night out north of 6th Street! Don''t waste this opportunity to get hammered with your coworkers or by yourself at these bars, where the drinks are cheaper than the people drinking them.','2021-03-27 23:51:44.856755+00','2021-03-27 23:51:44.856755+00');
SELECT pg_catalog.setval('public.packages_id_seq', 4, true);
ALTER TABLE ONLY public.package_tags ADD CONSTRAINT package_tags_package__id_tag__name_key UNIQUE (package__id, tag__name);
ALTER TABLE ONLY public.packages ADD CONSTRAINT packages_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.packages ADD CONSTRAINT packages_slug_key UNIQUE (slug);
ALTER TABLE ONLY public.package_tags ADD CONSTRAINT package_tags_package__id_fkey FOREIGN KEY (package__id) REFERENCES public.packages(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.package_tags ADD CONSTRAINT package_tags_tag__name_fkey FOREIGN KEY (tag__name) REFERENCES public.tags(name) ON DELETE CASCADE;
ALTER TABLE ONLY public.wishlists ADD CONSTRAINT wishlists_package__id_fkey FOREIGN KEY (package__id) REFERENCES public.packages(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.wishlists ADD CONSTRAINT wishlists_user__id_fkey FOREIGN KEY (user__id) REFERENCES public.users(id) ON DELETE CASCADE;
